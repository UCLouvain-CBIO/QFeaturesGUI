Shiny.addCustomMessageHandler("initWorkflowSortable", function(msg) {

  const palette  = document.getElementById(msg.palette);
  const workflow = document.getElementById(msg.workflow);

  if (!palette || !workflow) return;

  function addDeleteBtn(el){
    const btn = document.createElement("span");
    btn.innerText = "x";
    btn.classList.add("delete-btn");
    el.appendChild(btn);
  }

  new Sortable(palette, {
    group: { name: "steps", pull: "clone", put: false },
    sort: false,
    animation: 150
  });

  new Sortable(workflow, {
    group: "steps",
    animation: 180,
    ghostClass: "sortable-ghost",
    onAdd: function(evt){
      addDeleteBtn(evt.item);
      send();
    },
    onSort: send
  });

  document
    .querySelectorAll("#" + msg.workflow + " .step")
    .forEach(addDeleteBtn);

  workflow.addEventListener("click", function(e){
    if(e.target.classList.contains("delete-btn")){
      e.target.parentNode.remove();
      send();
    }
  });

  function send(){
    const steps = [...workflow.querySelectorAll(".step")]
      .map(el => el.dataset.step);
    Shiny.setInputValue(msg.input, steps, { priority: "event" });
  }

  send(); // initial value
});

