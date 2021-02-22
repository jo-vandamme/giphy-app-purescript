"use strict";

exports.getGridStyleImpl = (just) => (nothing) => (el) => () => {
  const style = getComputedStyle(el);
  const gridAutoRows = parseInt(style.getPropertyValue("grid-auto-rows"));
  const gridRowGap = parseInt(style.getPropertyValue("grid-row-gap"));
  return isFinite(gridAutoRows) && isFinite(gridRowGap)
    ? just({
        gridAutoRows,
        gridRowGap,
      })
    : nothing;
};
