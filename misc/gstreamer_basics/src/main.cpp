#include <gst/gst.h>
#include <iostream>

int main(int argc, char *argv[]) {
    if (argc < 2) {
        std::cout << "player <video-file>\n";
        return -1;
    }

    const char *filepath = argv[1];

    gst_init(&argc, &argv);

    GstElement *pipeline = gst_pipeline_new("video-player"); // root

    /// gst_element_factory_make(plugin name, instance name)
    GstElement *source = gst_element_factory_make("filesrc", "file-source");
    GstElement *decode = gst_element_factory_make("decodebin", "decoder"); // this also does auto detection of container/codex.
    GstElement *convert = gst_element_factory_make("videoconvert", "converter");
    GstElement *sink = gst_element_factory_make("autovideosink", "video-output");

    if (!pipeline || !source || !decode || !convert || !sink) {
        std::cerr << "failed to create elements\n";
        return -1;
    }

    g_object_set(G_OBJECT(source), "location", filepath, NULL); // i.e. set location property on source to be filepath. GLib-isms with gst macros
    gst_bin_add_many(GST_BIN(pipeline), source, decode, convert, sink, NULL); // all to pipeline bin

    // static links. only decode to convert is dynamic
    if (!gst_element_link(source, decode)) {
        std::cerr << "source and decodebin could not be linked\n";
        gst_object_unref(pipeline);
        return -1;
    }
    if (!gst_element_link(convert, sink)) {
        std::cerr << "convert and sink could not be linked\n";
        gst_object_unref(pipeline);
        return -1;
    }

    // to link source pad and sink pad
    g_signal_connect(
        decode,
        "pad-added",
        G_CALLBACK(+[](GstElement *src, GstPad *new_pad, gpointer user_data) {
            GstElement *convert = (GstElement *)user_data;
            GstPad *sink_pad = gst_element_get_static_pad(convert, "sink");

            if (gst_pad_is_linked(sink_pad)) {
                gst_object_unref(sink_pad);
                return;
            }

            GstPadLinkReturn ret = gst_pad_link(new_pad, sink_pad);
            if (GST_PAD_LINK_FAILED(ret)) {
                std::cerr << "pad link failed\n";
            }

            gst_object_unref(sink_pad);
        }),
        convert);

    // to play
    GstStateChangeReturn ret = gst_element_set_state(pipeline, GST_STATE_PLAYING);
    if (ret == GST_STATE_CHANGE_FAILURE) {
        std::cerr << "unable to set pipeline to playing\n";
        gst_object_unref(pipeline);
        return -1;
    }

    // pipeline's message bus
    GstBus *bus = gst_element_get_bus(pipeline);
    bool terminate = false;

    while (!terminate) {
        GstMessage *msg = gst_bus_timed_pop_filtered(
            bus,
            GST_CLOCK_TIME_NONE,
            (GstMessageType)(GST_MESSAGE_ERROR | GST_MESSAGE_EOS)
        );

        if (msg != NULL) {
            GError *err;
            gchar *debug_info;

            switch (GST_MESSAGE_TYPE(msg)) {
            case GST_MESSAGE_ERROR:
                gst_message_parse_error(msg, &err, &debug_info);
                std::cerr << "Error: " << err->message << "\n";
                g_clear_error(&err);
                g_free(debug_info);
                terminate = true;
                break;

            case GST_MESSAGE_EOS:
                std::cout << "end of stream\n";
                terminate = true;
                break;

            default:
                break;
            }

            gst_message_unref(msg);
        }
    }

    gst_object_unref(bus);
    gst_element_set_state(pipeline, GST_STATE_NULL);
    gst_object_unref(pipeline);

    return 0;
}
