interface_module_workflow_config_tab <- function(id) {
    tagList(
        uiOutput(NS(id, "tabs")),
        actionButton(NS(id, "add"), "Add Step"),
        actionButton(NS(id, "remove"), "Remove Last Step"),
        actionButton(NS(id, "apply"), "Confirm Current Workflow")
    )
}
