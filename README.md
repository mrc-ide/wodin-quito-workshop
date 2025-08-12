## Infectious Disease Modelling and Analytics for Pandemic Preparedness (IDMAPP): A focus on Latin America

For more information, see [infectiousdiseasemodels.org](http://www.infectiousdiseasemodels.org/) or [to the applications]()

Automated screenshots are generated in a github workflow named `screenshots` which runs a suite of Playwright tests. To
run these locally:
1. Run WODIN with config: `./scripts/run-wodin`
1. Make playwright suite the local folder: `cd screenshot`
1. Install Playwright: `npm install -D @playwright/test && npx playwright install`
1. Run the tests: `npx playwright test`
