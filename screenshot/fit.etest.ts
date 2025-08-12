import {test, expect, Page} from "@playwright/test";
import {takeScreenshot} from "./utils";

const folder = "fit";

test.beforeEach(async ({ page }) => {
    await page.goto("/apps/flu");
});

const uploadData = async (page: Page) => {
    const filename = "../files/flu/influenza_data.csv";
    await page.setInputFiles("#fitDataUpload", filename);
};

test("Data and Run tabs", async ({ page }) => {
    await uploadData(page);
    // wait for Run tab to update with Cases series
    await expect(page.locator(".wodin-plot-data-summary-series[name='Cases']")).toHaveCount(1);
    await takeScreenshot(page, folder,"DataRun");
});

test("Options and Fit tabs", async ({ page }) => {
    await uploadData(page);
    await page.click(":nth-match(.wodin-left .nav-link, 3)");
    await page.click(":nth-match(.wodin-right .nav-link, 2)");
    await page.locator("#link-data select").selectOption("onset"); // link data to variable
    // parameters to vary
    await page.locator(":nth-match(#model-params .form-check-input, 1)").check();
    await page.locator(":nth-match(#model-params .form-check-input, 3)").check();
    await page.click("#fit-btn");
    // wait for fit complete check
    await expect(page.locator(".fit-tab i svg.feather-check")).toHaveCount(1);
    await takeScreenshot(page, folder,"OptionsFit");
});