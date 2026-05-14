package np.com.sudanchapagain

import javax.swing.*
import java.awt.*
import javax.swing.event.{DocumentListener, DocumentEvent}

private val TypingText: String =
  "Meow Meow biralo yeta tira aau " +
    "Mero nana kaati khane musa mari khau."

@main
def app(): Unit =
  SwingUtilities.invokeLater(() => appUI())
  Thread.currentThread().join()

def appUI(): Unit = {
  try {
    val frame = new JFrame("type")
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.setLayout(new BorderLayout())

    val showArea = textArea(TypingText, editable = false)
    showArea.setFocusable(false)

    val typeArea = textArea("", editable = true)

    val elapsedField = textField()
    val errorsField  = textField()
    val wpmField     = textField()

    var startTime    = 0L
    var timer: Timer = null

    def updateStats(): Unit = {
      if startTime == 0L then return

      val now     = System.currentTimeMillis()
      val seconds = ((now - startTime + 500) / 1000).toInt.max(1)
      val typed   = typeArea.getText
      val errors  = countErrors(TypingText, typed)
      val minutes = seconds / 60.0
      val wpm     = ((typed.length / 5.0) / minutes).toInt

      elapsedField.setText(seconds.toString)
      errorsField.setText(errors.toString)
      wpmField.setText(wpm.toString)
    }

    typeArea.getDocument.addDocumentListener(simpleDocListener(() => {
      val typed = typeArea.getText

      if startTime == 0L && !typed.isBlank then
        startTime = System.currentTimeMillis()
        timer = new Timer(500, _ => updateStats())
        timer.start()

      if startTime != 0L && typed == TypingText then
        if timer != null then timer.stop()
        updateStats()
    }))

    val showScroll = new JScrollPane(showArea)
    val typeScroll = new JScrollPane(typeArea)

    val center = new JPanel(new GridLayout(2, 1))
    center.add(showScroll)
    center.add(typeScroll)

    val stats = new JPanel(new FlowLayout())
    stats.add(new JLabel("Time:"))
    stats.add(elapsedField)
    stats.add(new JLabel("Errors:"))
    stats.add(errorsField)
    stats.add(new JLabel("WPM:"))
    stats.add(wpmField)

    frame.add(center, BorderLayout.CENTER)
    frame.add(stats, BorderLayout.SOUTH)

    syncScroll(showScroll, typeScroll)

    frame.pack()
    frame.setLocationRelativeTo(null)
    frame.setVisible(true)

  } catch {
    case e: Throwable =>
      e.printStackTrace()
  }
}

private def textArea(text: String, editable: Boolean): JTextArea = {
  val ta = new JTextArea(text, 10, 40)
  ta.setEditable(editable)
  ta.setLineWrap(true)
  ta.setWrapStyleWord(true)
  ta
}

private def textField(): JTextField = {
  val tf = new JTextField(5)
  tf.setEditable(false)
  tf
}

private def simpleDocListener(onChange: () => Unit): DocumentListener = {
  new DocumentListener {
    def insertUpdate(e: DocumentEvent): Unit  = onChange()
    def removeUpdate(e: DocumentEvent): Unit  = onChange()
    def changedUpdate(e: DocumentEvent): Unit = onChange()
  }
}

private def countErrors(expected: String, actual: String): Int = {
  val minLen = expected.length.min(actual.length)

  val mismatches =
    expected
      .substring(0, minLen)
      .zip(actual.substring(0, minLen))
      .count(_ != _)

  mismatches + (expected.length - actual.length).abs
}

private def syncScroll(a: JScrollPane, b: JScrollPane): Unit = {
  val v1 = a.getVerticalScrollBar
  val v2 = b.getVerticalScrollBar

  var adjusting = false

  v1.addAdjustmentListener { e =>
    if !adjusting then
      adjusting = true
      v2.setValue(e.getValue)
      adjusting = false
  }

  v2.addAdjustmentListener { e =>
    if !adjusting then
      adjusting = true
      v1.setValue(e.getValue)
      adjusting = false
  }
}
