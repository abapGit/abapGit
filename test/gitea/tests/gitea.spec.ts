import { test, expect } from '@playwright/test';

test('do initial configuration', async ({ page }) => {
  await page.goto('http://localhost:3050/');
  await page.getByText('Administrator Account Settings').click();
  await page.getByLabel('Administrator Username').fill('abapgit');
  await page.getByLabel('Email Address', { exact: true }).fill('abapgit@abapgit.org');
  await page.getByRole('textbox', { name: 'Password', exact: true }).fill('abapgit');
  await page.getByLabel('Confirm Password').fill('abapgit');
  await page.getByRole('button', { name: 'Install Gitea' }).click();
  await page.goto('http://localhost:3050/');
});
