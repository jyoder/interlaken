function startHotReloadMonitor() {
  const host = window.location.host;
  const url = `ws://${host}`;

  const initialRestartBackoff = 0.1;
  const maxRestartBackoff = 20;

  let status = "initialized";
  let webSocket = null;
  let restartBackoff = initialRestartBackoff;
  let nextRestart = new Date();

  setInterval(() => {
    if (status === "initialized") {
      console.log("hot-reload: establishing websocket connection");
      status = "starting";

      webSocket = new WebSocket(url);
      webSocket.onopen = () => {
        console.log("hot-reload: connection established");
        status = "started";
      };

      webSocket.onclose = () => {
        if (status !== "starting" && status !== "started") {
          return;
        }
        console.log("hot-reload: websocket disconnected");
        status = "disconnected";
      };
    } else if (status === "disconnected") {
      const now = new Date();
      if (nextRestart > now) {
        return;
      }
      console.log("hot-reload: attempting websocket reconnect");
      status = "restarting";

      webSocket = new WebSocket(url);
      nextRestart = addSeconds(now, restartBackoff);
      const nextBackoff = restartBackoff * 2;
      restartBackoff = nextBackoff > maxRestartBackoff ? maxRestartBackoff : nextBackoff;

      webSocket.onopen = () => {
        if (status !== "restarting") {
          return;
        }

        console.log("hot-reload: connection re-established, reloading page");
        status = "restarted";
        window.location.reload();
      };

      webSocket.onclose = () => {
        if (status !== "restarting" && status !== "restarted") {
          return;
        }
        console.log(
          `hot-reload: websocket disconnected, attempting reconnect in ${restartBackoff} seconds`
        );
        status = "disconnected";
      };
    }
  }, 100);

  window.onbeforeunload = (_) => {
    if (webSocket != null) {
      webSocket.close();
    }
  };

  function addSeconds(date, seconds) {
    const newDate = new Date();
    return newDate.setSeconds(date.getSeconds() + seconds);
  }
}
