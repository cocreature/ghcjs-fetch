const fs = require("fs");
const webdriver = require("selenium-webdriver");
const chromedriver = require("chromedriver");

const chromeCapabilities = webdriver.Capabilities.chrome();
chromeCapabilities.set("chromeOptions", {
  binary: "/usr/bin/google-chrome-stable",
  args: ["--headless", "--enable-logging"]
});

const loggingPrefs = new webdriver.logging.Preferences();
loggingPrefs.setLevel("browser", webdriver.logging.Level.ALL);

const driver = new webdriver.Builder()
  .forBrowser("chrome")
  .withCapabilities(chromeCapabilities)
  .setLoggingPrefs(loggingPrefs)
  .build();

driver.get("http://www.google.com");

fs.readFile(
  "dist-newstyle/build/x86_64-linux/ghcjs-0.2.1/ghcjs-fetch-0.1.0.0/c/ghcjs-fetch-test/noopt/build/ghcjs-fetch-test/ghcjs-fetch-test.jsexe/all.js",
  "utf8",
  (err, data) => {
    if (err) {
      console.log(err);
    }
    driver.manage().timeouts().setScriptTimeout(10000).then(() => {
      driver.executeAsyncScript(
        "window.seleniumCallback = arguments[arguments.length - 1];\n" + data
      );
      driver.manage().logs().get("browser").then(function(entries) {
        const logRegex = /^console-api \d+:\d+ "(.*)"$/;
        entries.forEach(entry => {
          process.stdout.write(
            entry.message.match(logRegex)[1].replace("\\n", "\n")
          );
        });
      });
    });
    driver.quit();
  }
);
