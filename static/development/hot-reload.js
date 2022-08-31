function startHotReloadMonitor() {
  const host = window.location.host;
  console.log(window.location.host);
  const webSocketUrl = `ws://${host}/hot_reload`;
  const pollUrl = `http://${host}/hot_reload`;
  const pollDelay = 0.1;
  const submitDelay = 5000;

  let status = "initialized";
  let webSocket = null;
  let nextPoll = new Date();
  let submitting = false;

  window.onsubmit = () => {
    submitting = true;
  };

  setInterval(() => {
    if (status === "initialized") {
      console.log("hot-reload: establishing websocket connection");
      status = "starting";

      webSocket = new WebSocket(webSocketUrl);
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
      if (nextPoll > now) {
        return;
      }
      status = "polling";
      nextPoll = addSeconds(now, pollDelay);
      console.log(`nextPoll=${nextPoll}, submitting=${submitting}`);
      fetch(pollUrl)
        .then((_) => {
          if (!submitting) {
            window.location.reload();
          } else {
            setTimeout(() => (submitting = false), submitDelay);
          }
        })
        .catch((_) => {
          status = "disconnected";
        });
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
