export const flags = ({ env }) => {
  return {
    token: JSON.parse(localStorage.token || null)
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

    if (app.ports.promptToSave) {
      app.ports.promptToSave.subscribe(b => {
        if (b) {
          addEventListener("beforeunload", listener, {capture: true});
        } else {
          removeEventListener("beforeunload", listener, {capture: true});
        }
      });
    }
  }
};

const listener = (e) => {
  e.preventDefault();
  return event.returnValue = "Are you sure you want to exit? You still have unsaved changes.";
};