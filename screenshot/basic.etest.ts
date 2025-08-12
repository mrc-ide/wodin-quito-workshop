import { test, expect } from "@playwright/test";
import {takeScreenshot, waitForPlot} from "./utils";

const folder = "basic";

test.beforeEach(async ({ page }) => {
    await page.goto("/apps/vaccination");
});

test("Code and Run tabs", async ({ page }) => {
    await waitForPlot(page);
    await takeScreenshot(page, folder, "CodeRun");
});

test("Options and Run tabs", async ({ page }) => {
    await page.click(":nth-match(.wodin-left .nav-link, 2)");
    await takeScreenshot(page, folder, "OptionsRun");
});

test("Options and Sensitivity tabs", async ({ page }) => {
    await page.click(":nth-match(.wodin-left .nav-link, 2)");
    await page.click(":nth-match(.wodin-right .nav-link, 2)");
    // choose a parameter with more obvious effect on plot
    await page.click("#sensitivity-options .btn-primary");
    await page.locator("#edit-param-to-vary select").selectOption("R0");
    await page.click("#edit-param .btn-primary"); //OK from dialog
    await page.click("#run-sens-btn");
    await expect(page.locator("#run-sens-btn")).toBeEnabled();
    await takeScreenshot(page, folder, "OptionsSensitivity");
});