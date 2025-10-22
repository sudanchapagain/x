import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.application.Application;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.chart.PieChart;
import javafx.scene.control.*;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import javafx.scene.text.Font;
import javafx.stage.Stage;
import javafx.util.Duration;

public class JavaFXExamples extends Application {

    @Override
    public void start(Stage primaryStage) {
        TabPane tabPane = new TabPane();

        tabPane.getTabs().add(new Tab("Basic Controls", createBasicControls()));
        tabPane.getTabs().add(new Tab("Layouts", createLayouts()));
        tabPane.getTabs().add(new Tab("Bindings", createBindingsExample()));
        tabPane.getTabs().add(new Tab("Events", createEventsExample()));
        tabPane.getTabs().add(new Tab("Animation", createAnimationExample()));
        tabPane.getTabs().add(new Tab("Charts", createChartsExample()));

        Scene scene = new Scene(tabPane, 800, 600);
        primaryStage.setTitle("JavaFX Examples");
        primaryStage.setScene(scene);
        primaryStage.show();
    }

    private Pane createBasicControls() {
        VBox vbox = new VBox(10);
        vbox.setPadding(new Insets(10));

        Label label = new Label("Enter Your Name:");
        TextField textField = new TextField();
        Button button = new Button("Greet Me");
        Label greetingLabel = new Label();

        button.setOnAction(e -> greetingLabel.setText("Hello, " + textField.getText() + "!"));

        vbox.getChildren().addAll(label, textField, button, greetingLabel);
        return vbox;
    }

    private Pane createLayouts() {
        BorderPane borderPane = new BorderPane();

        HBox top = new HBox();
        top.setStyle("-fx-background-color: lightblue;");
        top.setPadding(new Insets(10));
        top.setAlignment(Pos.CENTER);
        top.getChildren().add(new Label("Top Section"));

        VBox left = new VBox(10);
        left.setStyle("-fx-background-color: lightgreen;");
        left.setPadding(new Insets(10));
        left.getChildren().addAll(new Button("Option 1"), new Button("Option 2"));

        GridPane center = new GridPane();
        center.setPadding(new Insets(10));
        center.setVgap(10);
        center.setHgap(10);
        center.add(new Label("Grid Cell [0,0]"), 0, 0);
        center.add(new Label("Grid Cell [1,1]"), 1, 1);

        borderPane.setTop(top);
        borderPane.setLeft(left);
        borderPane.setCenter(center);

        return borderPane;
    }

    private Pane createBindingsExample() {
        VBox vbox = new VBox(10);
        vbox.setPadding(new Insets(10));

        Slider slider = new Slider(0, 100, 50);
        Label label = new Label();
        label.textProperty().bind(slider.valueProperty().asString("%.2f"));

        vbox.getChildren().addAll(new Label("Adjust the slider:"), slider, label);
        return vbox;
    }

    private Pane createEventsExample() {
        VBox vbox = new VBox(10);
        vbox.setPadding(new Insets(10));

        Button button = new Button("Click Me");
        Label label = new Label("Button not clicked.");

        button.setOnMouseClicked(e -> label.setText("Button clicked!"));

        vbox.getChildren().addAll(button, label);
        return vbox;
    }

    private Pane createAnimationExample() {
        Pane pane = new Pane();
        Circle circle = new Circle(50, Color.BLUE);
        circle.setCenterX(100);
        circle.setCenterY(100);

        Timeline timeline = new Timeline(
                new KeyFrame(Duration.ZERO, e -> circle.setCenterX(circle.getCenterX() + 2)),
                new KeyFrame(Duration.millis(20))
        );
        timeline.setCycleCount(Timeline.INDEFINITE);
        timeline.play();

        pane.getChildren().add(circle);
        return pane;
    }

    private Pane createChartsExample() {
        ObservableList<PieChart.Data> pieChartData = FXCollections.observableArrayList(
                new PieChart.Data("Java", 25),
                new PieChart.Data("Python", 35),
                new PieChart.Data("JavaScript", 40)
        );

        PieChart pieChart = new PieChart(pieChartData);
        pieChart.setTitle("Programming Language Popularity");

        return pieChart;
    }

    public static void main(String[] args) {
        launch(args);
    }
}
