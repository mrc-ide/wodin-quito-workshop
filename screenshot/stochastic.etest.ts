import {test, expect, Page} from "@playwright/test";
import {takeScreenshot, waitForPlot} from "./utils";

const folder = "stochastic";

test.beforeEach(async ({ page }) => {
    await page.goto("/apps/stochasticity-3");
});

test("Code and Run tabs", async ({ page }) => {
    await page.click(":nth-match(.wodin-right .nav-link, 2)");
    await waitForPlot(page);
    await takeScreenshot(page, folder, "CodeRun");
});

test("Options and Run tabs", async ({ page }) => {
    await page.click(":nth-match(.wodin-left .nav-link, 2)");
    await page.click(":nth-match(.wodin-right .nav-link, 2)");
    await waitForPlot(page);
    await takeScreenshot(page, folder, "OptionsRun");
});