package ex2024travel.lofi_acl.permission_pane

import scalafx.beans.property.BooleanProperty
import scalafx.scene.control.CheckBox

class PermissionSelectionViewModel private (
    val children: Map[String, PermissionSelectionViewModel],
    val uiText: String,
    val hardcodedReadSelected: Boolean = false,
    val hardcodedWriteSelected: Boolean = false,
    val hardcodedReadDisabled: Boolean = false,
    val hardcodedWriteDisabled: Boolean = false
) {
  private var parent: Option[PermissionSelectionViewModel] = None
  val (
    readCheckbox: CheckBox,
    readSelected: BooleanProperty,
    readDisabled: BooleanProperty,
    readIndeterminate: BooleanProperty
  ) = {
    def updateParentsIndeterminateState(newVal: Boolean): Unit = {
      parent.foreach { p =>
        // switched to selected and parent is not selected and parent is not indeterminate
        if newVal && !p.readSelected.value && !p.readIndeterminate.value then
          p.readIndeterminate.value = true
        if !newVal && !p.readSelected.value && p.readIndeterminate.value then {
          // Check if all siblings are unset and not indeterminate
          if p.children.values.forall(c => !c.readSelected.value && !c.readIndeterminate.value) then
            p.readIndeterminate.value = false
        }
      }
    }
    val cb = CheckBox()
    if hardcodedReadDisabled then cb.disable = true
    if hardcodedReadSelected then cb.selected = true
    cb.selected.onChange { (_, oldVal, newVal) =>
      if !hardcodedReadSelected && newVal then readIndeterminate.value = false
      children.foreach { (_, childModel) =>
        if !childModel.hardcodedReadSelected then childModel.readSelected.value = newVal
        if !childModel.hardcodedReadDisabled then childModel.readDisabled.value = newVal
      }
    }
    cb.selected.onChange { (_, _, newVal) => updateParentsIndeterminateState(newVal) }
    cb.indeterminate.onChange { (_, _, newVal) => updateParentsIndeterminateState(newVal) }
    (cb, cb.selected, cb.disable, cb.indeterminate)
  }
  val (
    writeCheckbox: CheckBox,
    writeSelected: BooleanProperty,
    writeDisabled: BooleanProperty,
    writeIndeterminate: BooleanProperty
  ) = {
    def updateParentsIndeterminateState(newVal: Boolean): Unit = {
      parent.foreach { p =>
        // switched to selected and parent is not selected and parent is not indeterminate
        if newVal && !p.writeSelected.value && !p.writeIndeterminate.value then
          p.writeIndeterminate.value = true
        if !newVal && !p.writeSelected.value && p.writeIndeterminate.value then {
          // Check if all siblings are unset and not indeterminate
          if p.children.values.forall(c => !c.writeSelected.value && !c.writeIndeterminate.value) then
            p.writeIndeterminate.value = false
        }
      }
    }
    val cb = CheckBox()
    cb.disable = hardcodedWriteDisabled
    cb.selected = hardcodedWriteSelected
    cb.selected.onChange { (_, _, newVal) =>
      // Update selection state and disabled state of read
      if !hardcodedReadSelected then {
        readSelected.value = newVal
        if newVal then readIndeterminate.value = false
      }
      if !hardcodedReadDisabled then readDisabled.value = newVal
      // Update child write selection state
      children.foreach { (_, childModel) =>
        if !childModel.hardcodedWriteSelected then childModel.writeSelected.value = newVal
        if !childModel.hardcodedWriteDisabled then childModel.writeDisabled.value = newVal
      }
    }
    // Update parent
    cb.selected.onChange { (_, _, newVal) => updateParentsIndeterminateState(newVal) }
    cb.indeterminate.onChange { (_, _, newVal) => updateParentsIndeterminateState(newVal) }
    (cb, cb.selected, cb.disable, cb.indeterminate)
  }

  private def bindParentsRecursively(parent: Option[PermissionSelectionViewModel]): Unit = {
    this.parent = parent
    children.foreach((_, child) => child.bindParentsRecursively(Some(this)))
  }
}

object PermissionSelectionViewModel {
  def fromSelector(selector: Selector[?]): PermissionSelectionViewModel = {
    def rec(label: String, selector: Selector[?]): PermissionSelectionViewModel = {
      val model = new PermissionSelectionViewModel(
        selector.children.map((childLabel, selector) => childLabel -> rec(childLabel, selector)),
        label
      )
      model
    }

    val model = rec("", selector)
    model.bindParentsRecursively(None)
    model
  }
}
