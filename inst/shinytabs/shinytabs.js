// Shiny input binding for the shinytabs tabset element.
//
// The server owns the tab list. It pushes the whole state (tabs in order,
// titles, icons, selection, tab types for the "+" menu, behaviour flags) in a
// single "shinytabs" custom message, and this binding reconciles the tab bar
// with it. Tab panes are inserted and removed by the server with insertUI()
// and removeUI(); the binding only shows the pane whose data-tab matches the
// selected id, keeping the others laid out but invisible so their contents
// keep their size.
//
// User intents travel back as <id>_event input events ({type, ...}), one of:
// select, close, add, reorder and rename. The element's own input value is
// just a readiness marker so the server knows when it can start sending.
//
// Colours come from Bootstrap 5 CSS variables when the page has them (so the
// bar follows bslib's colour mode) with plain fallbacks otherwise.
(function () {
  if (typeof Shiny === "undefined") return;

  var binding = new Shiny.InputBinding();

  function emit(st, ev) {
    st.n += 1;
    ev.n = st.n;
    Shiny.setInputValue(st.id + "_event", ev, { priority: "event" });
  }

  function closest(target, selector, root) {
    var el = target;
    while (el && el !== root) {
      if (el.matches && el.matches(selector)) return el;
      el = el.parentNode;
    }
    return null;
  }

  function tabEl(st, id) {
    return st.tabsEl.querySelector('.shinytabs-tab[data-id="' + id + '"]');
  }

  function tabIds(st) {
    return Array.prototype.map.call(
      st.tabsEl.querySelectorAll(".shinytabs-tab"),
      function (t) {
        return t.dataset.id;
      }
    );
  }

  function makeTab(st, tab) {
    var el = document.createElement("div");
    el.className = "shinytabs-tab";
    el.setAttribute("role", "tab");
    el.dataset.id = tab.id;

    var icon = document.createElement("span");
    icon.className = "shinytabs-tab-icon";
    var title = document.createElement("span");
    title.className = "shinytabs-tab-title";
    var close = document.createElement("button");
    close.type = "button";
    close.className = "shinytabs-tab-close";
    close.setAttribute("aria-label", "Close tab");
    close.setAttribute("title", "Close");
    close.tabIndex = -1;
    close.innerHTML = "&times;";

    el.appendChild(icon);
    el.appendChild(title);
    el.appendChild(close);
    return el;
  }

  function updateTab(st, el, tab, selected) {
    var icon = el.querySelector(".shinytabs-tab-icon");
    var title = el.querySelector(".shinytabs-tab-title");
    var close = el.querySelector(".shinytabs-tab-close");

    var iconHtml = tab.icon || "";
    if (icon.innerHTML !== iconHtml) icon.innerHTML = iconHtml;
    icon.hidden = !iconHtml;

    if (!el.classList.contains("renaming") && title.textContent !== tab.title) {
      title.textContent = tab.title;
    }
    el.setAttribute("title", tab.title);

    close.hidden = !tab.closeable;
    el.dataset.closeable = tab.closeable ? "true" : "false";
    el.draggable = !!st.reorderable;

    var active = tab.id === selected;
    el.classList.toggle("active", active);
    el.setAttribute("aria-selected", active ? "true" : "false");
    el.tabIndex = active ? 0 : -1;
  }

  function renderMenu(st, types) {
    st.types = types || [];
    st.menu.innerHTML = "";
    st.types.forEach(function (t) {
      var item = document.createElement("button");
      item.type = "button";
      item.className = "shinytabs-menu-item";
      item.dataset.key = t.key;
      item.textContent = t.label;
      st.menu.appendChild(item);
    });
    st.addEl.hidden = st.types.length === 0;
    st.addBtn.setAttribute("aria-haspopup", st.types.length > 1 ? "menu" : "false");
    if (st.types.length === 1) {
      st.addBtn.setAttribute("title", "New " + st.types[0].label);
    } else {
      st.addBtn.setAttribute("title", "New tab");
    }
  }

  function showPanes(st, selected) {
    var panes = st.panesEl.querySelectorAll(":scope > .shinytabs-pane");
    Array.prototype.forEach.call(panes, function (p) {
      p.classList.toggle("active", p.dataset.tab === selected);
    });
  }

  function render(st, state) {
    st.reorderable = !!state.reorderable;
    st.renameable = !!state.renameable;
    renderMenu(st, state.types);

    var tabs = state.tabs || [];
    var seen = {};
    tabs.forEach(function (tab) {
      seen[tab.id] = true;
      var el = tabEl(st, tab.id) || makeTab(st, tab);
      updateTab(st, el, tab, state.selected);
      st.tabsEl.appendChild(el);
    });
    Array.prototype.forEach.call(
      st.tabsEl.querySelectorAll(".shinytabs-tab"),
      function (el) {
        if (!seen[el.dataset.id]) el.remove();
      }
    );

    var changed = st.selected !== state.selected;
    st.selected = state.selected || null;
    showPanes(st, st.selected);
    st.el.classList.toggle("empty", tabs.length === 0);

    if (st.selected) {
      var active = tabEl(st, st.selected);
      if (active && active.scrollIntoView) {
        active.scrollIntoView({ block: "nearest", inline: "nearest" });
      }
    }
    if (changed) {
      st.el.dispatchEvent(
        new CustomEvent("shinytabs:selected", {
          bubbles: true,
          detail: { id: st.selected }
        })
      );
    }
  }

  function selectLocal(st, id) {
    if (!id || id === st.selected) return;
    Array.prototype.forEach.call(
      st.tabsEl.querySelectorAll(".shinytabs-tab"),
      function (el) {
        var active = el.dataset.id === id;
        el.classList.toggle("active", active);
        el.setAttribute("aria-selected", active ? "true" : "false");
        el.tabIndex = active ? 0 : -1;
      }
    );
    st.selected = id;
    showPanes(st, id);
    emit(st, { type: "select", id: id });
  }

  function requestClose(st, el) {
    if (el.dataset.closeable !== "true") return;
    emit(st, { type: "close", id: el.dataset.id });
  }

  function hideMenu(st) {
    st.menu.hidden = true;
    st.addBtn.setAttribute("aria-expanded", "false");
  }

  function toggleMenu(st) {
    var open = st.menu.hidden;
    st.menu.hidden = !open;
    st.addBtn.setAttribute("aria-expanded", open ? "true" : "false");
    if (open) {
      var first = st.menu.querySelector(".shinytabs-menu-item");
      if (first) first.focus();
    }
  }

  function startRename(st, el) {
    if (!st.renameable || el.classList.contains("renaming")) return;
    var title = el.querySelector(".shinytabs-tab-title");
    var old = title.textContent;
    var input = document.createElement("input");
    input.type = "text";
    input.className = "shinytabs-rename";
    input.value = old;
    input.setAttribute("aria-label", "Tab title");
    input.style.width = Math.max(4, old.length + 2) + "ch";
    el.classList.add("renaming");
    el.draggable = false;
    title.textContent = "";
    title.appendChild(input);
    input.focus();
    input.select();

    var done = false;
    var finish = function (commit) {
      if (done) return;
      done = true;
      var value = input.value.trim();
      el.classList.remove("renaming");
      el.draggable = !!st.reorderable;
      title.textContent = commit && value ? value : old;
      if (commit && value && value !== old) {
        emit(st, { type: "rename", id: el.dataset.id, title: value });
      }
      el.focus();
    };
    input.addEventListener("keydown", function (e) {
      if (e.key === "Enter") {
        e.preventDefault();
        finish(true);
      } else if (e.key === "Escape") {
        e.preventDefault();
        finish(false);
      }
      e.stopPropagation();
    });
    input.addEventListener("blur", function () {
      finish(true);
    });
  }

  function clearDropMarks(st) {
    Array.prototype.forEach.call(
      st.tabsEl.querySelectorAll(".drop-before, .drop-after"),
      function (el) {
        el.classList.remove("drop-before", "drop-after");
      }
    );
  }

  function wire(st) {
    var tabs = st.tabsEl;

    tabs.addEventListener("click", function (e) {
      var el = closest(e.target, ".shinytabs-tab", tabs);
      if (!el) return;
      if (closest(e.target, ".shinytabs-tab-close", el)) {
        e.preventDefault();
        requestClose(st, el);
        return;
      }
      if (el.classList.contains("renaming")) return;
      selectLocal(st, el.dataset.id);
    });

    // Middle click closes, as in browsers and editors.
    tabs.addEventListener("auxclick", function (e) {
      if (e.button !== 1) return;
      var el = closest(e.target, ".shinytabs-tab", tabs);
      if (!el) return;
      e.preventDefault();
      requestClose(st, el);
    });

    tabs.addEventListener("dblclick", function (e) {
      var el = closest(e.target, ".shinytabs-tab", tabs);
      if (!el || closest(e.target, ".shinytabs-tab-close", el)) return;
      startRename(st, el);
    });

    tabs.addEventListener("keydown", function (e) {
      var el = closest(e.target, ".shinytabs-tab", tabs);
      if (!el || el.classList.contains("renaming")) return;
      var ids = tabIds(st);
      var i = ids.indexOf(el.dataset.id);
      var next = null;
      if (e.key === "ArrowRight") next = ids[(i + 1) % ids.length];
      else if (e.key === "ArrowLeft") next = ids[(i - 1 + ids.length) % ids.length];
      else if (e.key === "Home") next = ids[0];
      else if (e.key === "End") next = ids[ids.length - 1];
      else if (e.key === "Delete") {
        e.preventDefault();
        requestClose(st, el);
        return;
      } else if (e.key === "F2") {
        e.preventDefault();
        startRename(st, el);
        return;
      }
      if (next) {
        e.preventDefault();
        selectLocal(st, next);
        var nextEl = tabEl(st, next);
        if (nextEl) nextEl.focus();
      }
    });

    // Drag and drop reordering: the dragged tab is dropped before or after
    // the tab under the pointer, depending on which half it is over.
    tabs.addEventListener("dragstart", function (e) {
      var el = closest(e.target, ".shinytabs-tab", tabs);
      if (!el || !st.reorderable) {
        e.preventDefault();
        return;
      }
      st.dragging = el.dataset.id;
      el.classList.add("dragging");
      e.dataTransfer.effectAllowed = "move";
      try {
        e.dataTransfer.setData("text/plain", el.dataset.id);
      } catch (err) {
        /* IE */
      }
    });
    tabs.addEventListener("dragover", function (e) {
      if (!st.dragging) return;
      e.preventDefault();
      e.dataTransfer.dropEffect = "move";
      clearDropMarks(st);
      var el = closest(e.target, ".shinytabs-tab", tabs);
      if (!el || el.dataset.id === st.dragging) return;
      var rect = el.getBoundingClientRect();
      var before = e.clientX < rect.left + rect.width / 2;
      el.classList.add(before ? "drop-before" : "drop-after");
    });
    tabs.addEventListener("dragleave", function (e) {
      if (!tabs.contains(e.relatedTarget)) clearDropMarks(st);
    });
    tabs.addEventListener("drop", function (e) {
      if (!st.dragging) return;
      e.preventDefault();
      var el = closest(e.target, ".shinytabs-tab", tabs);
      var dragged = tabEl(st, st.dragging);
      clearDropMarks(st);
      if (!el || !dragged || el === dragged) return;
      var rect = el.getBoundingClientRect();
      var before = e.clientX < rect.left + rect.width / 2;
      tabs.insertBefore(dragged, before ? el : el.nextSibling);
      emit(st, { type: "reorder", ids: tabIds(st) });
    });
    tabs.addEventListener("dragend", function () {
      var dragged = st.dragging && tabEl(st, st.dragging);
      if (dragged) dragged.classList.remove("dragging");
      st.dragging = null;
      clearDropMarks(st);
    });

    st.addBtn.addEventListener("click", function (e) {
      e.preventDefault();
      if (st.types.length === 1) {
        emit(st, { type: "add", key: st.types[0].key });
      } else if (st.types.length > 1) {
        toggleMenu(st);
      }
    });
    st.menu.addEventListener("click", function (e) {
      var item = closest(e.target, ".shinytabs-menu-item", st.menu);
      if (!item) return;
      hideMenu(st);
      emit(st, { type: "add", key: item.dataset.key });
    });
    st.menu.addEventListener("keydown", function (e) {
      var items = st.menu.querySelectorAll(".shinytabs-menu-item");
      var i = Array.prototype.indexOf.call(items, document.activeElement);
      if (e.key === "ArrowDown") {
        e.preventDefault();
        items[(i + 1) % items.length].focus();
      } else if (e.key === "ArrowUp") {
        e.preventDefault();
        items[(i - 1 + items.length) % items.length].focus();
      } else if (e.key === "Escape") {
        e.preventDefault();
        hideMenu(st);
        st.addBtn.focus();
      }
    });
    document.addEventListener("click", function (e) {
      if (!st.menu.hidden && !st.addEl.contains(e.target)) hideMenu(st);
    });
  }

  $.extend(binding, {
    find: function (scope) {
      return $(scope).find(".shinytabs");
    },

    initialize: function (el) {
      var st = {
        id: el.id,
        el: el,
        tabsEl: el.querySelector(".shinytabs-tabs"),
        addEl: el.querySelector(".shinytabs-add"),
        addBtn: el.querySelector(".shinytabs-add-btn"),
        menu: el.querySelector(".shinytabs-menu"),
        panesEl: el.querySelector(".shinytabs-panes"),
        types: [],
        selected: null,
        reorderable: false,
        renameable: false,
        dragging: null,
        n: 0,
        seq: 0,
        callback: null
      };
      el._shinytabs = st;
      st.menu.hidden = true;
      el.classList.add("empty");
      wire(st);
    },

    getValue: function (el) {
      return { ready: true, seq: el._shinytabs.seq };
    },

    subscribe: function (el, callback) {
      el._shinytabs.callback = callback;
    },

    unsubscribe: function (el) {
      el._shinytabs.callback = null;
    },

    receiveMessage: function (el, msg) {
      var st = el._shinytabs;
      if (msg.state) render(st, msg.state);
      if (msg.focus && st.selected) {
        var active = tabEl(st, st.selected);
        if (active) active.focus();
      }
    },

    getRatePolicy: function () {
      return { policy: "direct" };
    }
  });

  Shiny.inputBindings.register(binding, "shinytabs.tabset");

  Shiny.addCustomMessageHandler("shinytabs", function (msg) {
    var el = document.getElementById(msg.id);
    if (el && el._shinytabs) binding.receiveMessage(el, msg);
  });
})();
