import js from "@eslint/js";
import globals from "globals";

export default [
  js.configs.recommended,
  {
    "languageOptions": {
      "parserOptions": {
        "ecmaVersion": 2015
      },
      "sourceType": "script",
      "globals": globals.browser,
    },
    "rules": {
      "quotes": [
        "error",
        "double",
        {
          "avoidEscape": true
        }
      ],
      "semi": [
        "error",
        "always",
        {
          "omitLastInOneLineBlock": true
        }
      ],
      "no-console": "off",
      "indent": [
        "error",
        2
      ],
      "no-trailing-spaces": [
        "error"
      ],
      "no-unused-vars": [
        "warn"
      ]
    }
  }
];
