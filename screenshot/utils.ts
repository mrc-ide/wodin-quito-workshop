import {expect, Page} from "@playwright/test";

export const takeScreenshot = async (page: Page, folder: string, name: string) => {
    await page.screenshot({ path: `screenshots/${folder}/${name}.png` });
};

export const waitForPlot = async (page: Page) => {
    await expect(page.locator(":nth-match(.plot .main-svg, 1)")).toBeVisible();
};