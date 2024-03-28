// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.window

import java.awt.{  Cursor, Dimension, Font }

import org.nlogo.api.{ Editable, MersenneTwisterFast, Options }
import org.nlogo.core.{ AgentKind, Button => CoreButton, I18N }
import org.nlogo.nvm.Procedure

class NewButtonWidget(random:MersenneTwisterFast) extends JobWidget(random)
        with Editable 
        with Events.JobRemovedEvent.Handler with Events.TickStateChangeEvent.Handler {

  import ButtonWidget._

  type WidgetModel = CoreButton

  private var buttonType: ButtonType = ButtonType.ObserverButton

  locally {
    //setBackground(InterfaceColors.BUTTON_BACKGROUND)
    org.nlogo.awt.Fonts.adjustDefaultFont(this)
  }

  // buttonType now controls the agentKind. no one should ever be setting
  // agentKind from outside of this class anyway.
  // the ui edits work through agent options, which now just set the button type
  override def kind = buttonType.agentKind
  override def agentKind(c:AgentKind) { /* ignoring, no one should call this. */ }
  def agentOptions = buttonType.toAgentOptions
  def agentOptions(newAgentOptions:Options[String]){
    if (newAgentOptions.chosenValue != this.agentOptions.chosenValue){
      this.buttonType = ButtonType(newAgentOptions.chosenValue)
      recompile()
    }
  }

  var foreverOn = false
  var goTime = false
  var setupFinished = false

  private var _buttonUp = true
  def buttonUp = _buttonUp
  def buttonUp_=(newButtonUp:Boolean){
    if(newButtonUp) foreverOn = false
    _buttonUp = newButtonUp
  }

  protected var _forever = false
  def forever = _forever
  def forever_=(newForever: Boolean){
    if(newForever != _forever){
      _forever = newForever
      stopping = false
      recompile()
    }
  }

  /// keyboard stuff
  private var _actionKey: Option[Char] = None
  def actionKey = _actionKey.getOrElse(0.toChar)
  def actionKey_=(newActionKey:Char) {
    _actionKey = newActionKey match {
      case 0 => None
      case _ => Some(newActionKey)
    }
  }
//  private def actionKeyString = _actionKey.map(_.toString).getOrElse("")

  private var _keyEnabled = false
  def keyEnabled = _keyEnabled
  def keyEnabled(newKeyEnabled:Boolean){
    if(_keyEnabled != newKeyEnabled){
      _keyEnabled = newKeyEnabled
 //     repaint()
    }
  }

  def keyTriggered(){
    if (error == null){
      buttonUp = false
      respondToClick(true)
    }
  }

  private def disabledWaitingForSetup = goTime && ! setupFinished

  private def respondToClick(inBounds: Boolean) {
    if(disabledWaitingForSetup){
      buttonUp = true
    }
    else if (error == null) {
      if (forever) {
        if (inBounds) {
          foreverOn = !foreverOn
          buttonUp = !foreverOn
          action()
        }
        else buttonUp = !foreverOn
      }
      else {
        buttonUp = true
        if (inBounds) action()
      }
    }
  }

  /// editability
  override def classDisplayName = I18N.gui.get("tabs.run.widgets.button")
  def propertySet = Properties.button

  /// compilation & jobs
  var running = false
  var stopping = false
  override def isButton = true
  override def isTurtleForeverButton = buttonType == ButtonType.TurtleButton && forever
  override def isLinkForeverButton = buttonType == ButtonType.LinkButton && forever
  private var _name = ""
  def name = _name
  def name_=(newName:String){
    _name = newName
    chooseDisplayName()
  }

  override def procedure_=(p: Procedure): Unit = {
    super.procedure_=(p)
  }

  def action() {
    if (error == null) {
      // warning, confusing code ahead. not sure if there's a
      // clearer way to write this hard to know without trying.
      // it looks like maybe the forever button and the once button
      // cases should be completely separate. that might help
      // - ST 4/28/10

      // comments below added by - JC 9/16/10

      // handle a click if this button is a forever button,
      // or its a once button thats not running
      // this means we don't process clicks on once buttons that are already running.
      if (forever || !running) {

        // if its a once button or a forever button that is running
        // signal to the user that the button is in the process of stopping.
        if (!forever || running) {
          setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR))
        }

        // if it's a forever button that is running, that means we need to stop it.
        if (forever && running) {
          // the mouseReleased method in ButtonWidget will have prematurely
          // popped the button back up... so first we immediately undo what it did
          foreverOn = true
          buttonUp = false
          // then we mark the job for stopping -- the button will pop back up
          // when the job stops
          stopping = true
        }
        else {
          // in this case, it could be a forever button, but its not running
          // or it could be a once button that is not running.
          // remember, we couldn't have gotten into this if statement
          // if it was a once button that was already running.
          // so we've definitely clicked on a button that was up, and its time to run it.
          stopping = false
          running = true
        }

        // http://ccl.northwestern.edu/netlogo/docs/programming.html#buttons :
        // Forever buttons keep running their code over and over again,
        // until either the code hits the stop command, or you press the button again to stop it.
        // If you stop the button, the code doesn't get interrupted.
        // The button waits until the code has finished, then pops up.

        // if this is a forever button that was running, was clicked, and is now up.
        if (forever && buttonUp) {
          new Events.RemoveJobEvent(this).raise(this)
        }
        // a forever button that was stopped with the stop command.
        else if (forever && !buttonUp && stopping) {
          new Events.JobStoppingEvent(this).raise(this)
        }
        // a forever button or a once button that is now down because
        // it was just clicked.  it needs to run.
        else {
          new Events.AddJobEvent(this, agents, procedure).raise(this)
        }
      }
    }
  }

  def handle(e: Events.JobRemovedEvent) {
    if (e.owner == this) {
      popUpStoppingButton()
    }
  }

  def handle(e: Events.TickStateChangeEvent) {
    setupFinished = e.tickCounterInitialized
    repaint()
  }

  def popUpStoppingButton() {
    buttonUp = true
    running = false
    stopping = false
    repaint()
    setCursor(null)
  }

  /// source code
  private def chooseDisplayName(): Unit = {
    if (name == "")
      displayName(getSourceName)
    else
      displayName(name)
  }

  // behold the mighty regular expression
  private def getSourceName: String = {
    (innerSource: String).trim.replaceAll("\\s+", " ")
  }

  override def innerSource_=(newInnerSource:String): Unit = {
    super.innerSource_=(newInnerSource)
    chooseDisplayName
  }

  def wrapSource: String = innerSource

  def wrapSource(newInnerSource:String) {
    if (newInnerSource != "" && newInnerSource != innerSource) {
      this.innerSource = newInnerSource
      recompile()
    }
  }

  def recompile(){
    val header = "to __button [] " + buttonType.toHeaderCode + (if(forever) " loop [ " else "")
    val footer = "\n" + // protect against comments
      (if(forever) "__foreverbuttonend ] " else "__done ") + "end"
    new Events.RemoveJobEvent(this).raise(this)
    source(header, innerSource, footer)
    chooseDisplayName()
  }

  /// sizing
  override def getMinimumSize = new Dimension(55, 33)
  override def getPreferredSize(font: Font) = {
    val size = getMinimumSize
    size.width = StrictMath.max(size.width, getFontMetrics(font).stringWidth(displayName) + 28)
    size.height = StrictMath.max(size.height,
      getFontMetrics(font).getMaxDescent() + getFontMetrics(font).getMaxAscent() + 12)
    size
  }

  // saving and loading
  override def model: WidgetModel = {
    val b              = getBoundsTuple
    val savedActionKey = if (actionKey == 0 || actionKey == ' ') None else Some(actionKey)
    CoreButton(
      display = name.potentiallyEmptyStringToOption,
      left = b._1, top = b._2, right = b._3, bottom = b._4,
      source    = innerSource.potentiallyEmptyStringToOption,
      forever   = forever,        buttonKind             = buttonType.agentKind,
      actionKey = savedActionKey, disableUntilTicksStart = goTime)
  }

  override def load(button: WidgetModel): Object = {
    forever = button.forever
    buttonType = ButtonType(button.buttonKind)

    button.actionKey.foreach(k => actionKey = k)

    goTime = button.disableUntilTicksStart
    name = button.display.optionToPotentiallyEmptyString

    button.source.foreach(wrapSource)

    setSize(button.right - button.left, button.bottom - button.top)
    chooseDisplayName()
    this
  }
}
