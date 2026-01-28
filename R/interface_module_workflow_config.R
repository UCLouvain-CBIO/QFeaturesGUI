#'
#' @title Workflow Configuration Module (interface)
#'
#' @param id The module id
#'
#' @return A Shiny module UI function
#' @rdname INTERNAL_interface_module_workflow_config
#' @keywords internal
#'
#' @importFrom shiny uiOutput actionButton tagList icon
#' @importFrom htmltools div tags
#'
#' @title Workflow Configuration Module (interface)
#' @param id module id
#' @keywords internal
interface_module_workflow_config_tab <- function(id) {
  tagList(
    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/sortablejs@1.15.0/Sortable.min.js"),
      tags$style(HTML("
        .panel { background: white; border-radius: 12px; padding: 15px;
                 box-shadow: 0 4px 12px rgba(0,0,0,0.08); }
        .step { background: #fff; border-radius: 10px; padding: 12px;
                margin-bottom: 8px; cursor: grab; font-weight: 500;
                box-shadow: 0 3px 8px rgba(0,0,0,0.08);
                display: flex; justify-content: space-between; align-items: center; }
        .workflow-drop { min-height: 200px; border: 2px dashed #c9cbe3;
                         border-radius: 12px; padding: 12px; background: #fafbff; }
        .delete-btn { cursor: pointer; color: red; font-weight: bold; }
        .sortable-ghost { opacity: 0.4; }
      "))
    ),

    fluidRow(
      column(
        4,
        div(class="panel",
            tags$h4("Available steps"),
            div(
              id = NS(id, "palette"),
              lapply(
                c("Samples Filtering",
                  "Features Filtering",
                  "Log Transformation",
                  "Normalisation"),
                function(s) div(class="step", `data-step` = s, s)
              )
            )
        )
      ),
      column(
        8,
        div(class="panel",
            tags$h4("Workflow"),
            div(
              id = NS(id, "workflow"),
              class = "workflow-drop",
              lapply(
                c("Samples Filtering", "Log Transformation"),
                function(s) {
                  div(class = "step", `data-step` = s, s)
                }
              )
            )

        )
      )
    ),

    actionButton(
      NS(id, "apply"),
      "Confirm Current Workflow",
      class = "load-button",
      width = "100%"
    ),
    tags$script(HTML(sprintf(
          "
          (function(){
              Shiny.addCustomMessageHandler('%s', function(message){

                const palette = document.getElementById(message.palette);
                const workflow = document.getElementById(message.workflow);

                function addDeleteBtn(el){
                  const btn = document.createElement('span');
                  btn.innerText = '✕';
                  btn.classList.add('delete-btn');
                  el.appendChild(btn);
                }

                new Sortable(palette, {
                  group: { name: 'steps', pull: 'clone', put: false },
                  sort: false,
                  animation: 150
                });

                new Sortable(workflow, {
                  group: 'steps',
                  animation: 180,
                  ghostClass: 'sortable-ghost',
                  onAdd: function(evt){
                    addDeleteBtn(evt.item);
                    send();
                  },
                  onSort: send
                });

                document
                  .querySelectorAll('#' + message.workflow + ' .step')
                  .forEach(addDeleteBtn);

                workflow.addEventListener('click', function(e){
                  if(e.target.classList.contains('delete-btn')){
                    e.target.parentNode.remove();
                    send();
                  }
                });

                function send(){
                  const steps = [...workflow.querySelectorAll('.step')]
                    .map(el => el.dataset.step);
                  Shiny.setInputValue(message.input, steps, {priority: 'event'});
                }

                send();
              });
            })();

          ",
          "initWorkflowSortable"
        )))

    
  )
}

