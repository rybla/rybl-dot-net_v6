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
