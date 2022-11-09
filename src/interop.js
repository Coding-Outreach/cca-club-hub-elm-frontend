export const flags = ({ env }) => {
  return {
    token: JSON.parse(localStorage.token || null),
  };
};

export const onReady = ({ app, env }) => {
  // PORTS
  if (app.ports) {
    if (app.ports.saveToLocalStorage) {
      app.ports.saveToLocalStorage.subscribe(({ key, value }) => {
        localStorage[key] = JSON.stringify(value);
      });
    }

    if (app.ports.updateWarnUnsavedChanges) {
      app.ports.updateWarnUnsavedChanges.subscribe((on) => {
        on
          ? addEventListener("beforeunload", listener, { capture: true })
          : removeEventListener("beforeunload", listener, { capture: true });
      });
    }
  }
};

const listener = (e) => {
  e.preventDefault();
  return (event.returnValue =
    "Are you sure you want to exit? You still have unsaved changes.");
};
