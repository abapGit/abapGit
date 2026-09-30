import js from "@eslint/js";
import globals from "globals";

export default [
  js.configs.recommended,
  {
    "languageOptions": {
      "parserOptions": {
        // The IE browser control of SAP GUI for Windows parses ES5 only. A single
        // arrow function, let/const or template literal stops the whole script
        // there, and the Node-based UI tests (test/ui) would not notice
        "ecmaVersion": 5
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
