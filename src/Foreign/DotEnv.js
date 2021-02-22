"use strict";

exports.getApiKeyImpl = (just) => (nothing) => () => {
  return process.env.GIPHY_API_KEY ? just(process.env.GIPHY_API_KEY) : nothing;
};
