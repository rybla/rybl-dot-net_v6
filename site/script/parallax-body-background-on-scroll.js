document.addEventListener("DOMContentLoaded", () => {
  const parallaxSpeed = 0.05;
  const horizontalVsVerticalSpeed = 0.25;

  const parallaxContainer = document.getElementById("background");

  window.addEventListener("scroll", () => {
    const offset = window.scrollY;
    const dx = offset * parallaxSpeed * horizontalVsVerticalSpeed;
    const dy = offset * parallaxSpeed;

    parallaxContainer.style.backgroundPosition = `-${dx}px -${dy}px`;
  });
});

// document.addEventListener("DOMContentLoaded", () => {
//   const clientWidth = document.body.clientWidth
//   const clientHeight = document.body.clientHeight
//   const displayElement = document.createElement('div');
//   displayElement.textContent = `Client Width: ${clientWidth}px, Client Height: ${clientHeight}px`;
//   displayElement.style.color = "white";
//   document.body.prepend(displayElement);
// });
