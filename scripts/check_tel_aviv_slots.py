#!/usr/bin/python

######################################################################
#  Check the Swedish embassy site for available slots.
#
#  2025-10-01 Wed
#  Dov Grobgeld <dov.grobgeld@gmail.com>
######################################################################
import json
from selenium import webdriver
from selenium.webdriver.support.ui import Select
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.keys import Keys
import time
import os
import pdb
import telegram
from telegram import Bot
import asyncio
import argparse

bot_token=json.load(open('/home/dov/git/GrobBot/bot_config.json'))['token']
dov_user_id = '76500923'

async def report_result_to_telegram(user_id, message):
  bot = Bot(token=bot_token)
  await bot.send_message(chat_id=user_id, text=message, parse_mode='HTML')

MV_URL = "https://www.migrationsverket.se/ansokanbokning/?0"
BOOK_URL = "https://www.migrationsverket.se/ansokanbokning/valjtyp?3&enhet=U1005&sprak=sv&callback=https://www.swedenabroad.se"

def find_browser():
  browsers = ["google-chrome", "chromium", "chromium-browser"]
  for browser in browsers:
    path = os.path.join("/usr/bin", browser)
    if os.path.isfile(path) and os.access(path, os.X_OK):
      return path
  raise RuntimeError('No browser was found!')

def print_selected(select_elem, label):
  selected = select_elem.first_selected_option
  print(f'Selected in {label}: "{selected.text.strip()}" (value={selected.get_attribute("value")})')

def verify_not_empty(select_elem, label):
  selected = select_elem.first_selected_option
  assert(len(selected.text.strip()))

def main():
  parser = argparse.ArgumentParser(description='Check for available slots')
  parser.add_argument('-d', '--debug',
                      dest='debug',
                      action='store_true',
                      help='Don\'t run headless')
  args = parser.parse_args()
  
  
  chrome_options = Options()
  chrome_options.binary_location = str(find_browser())
  if not args.debug:
    chrome_options.add_argument("--headless")
  driver = webdriver.Chrome(options=chrome_options)

  try:
    driver.get(MV_URL)
    wait = WebDriverWait(driver, 20)

    # Select "EMBASSY OF SWEDEN IN TEL AVIV" in first combo box
    print('Choosing embassy: "TEL AVIV"')
    select1_elem = wait.until(EC.presence_of_element_located((By.ID, "mottagningsenhet")))
    select1 = Select(select1_elem)

    # Must do "key interaction" for the site to accept select_by_visible_text
    select1_elem.send_keys(Keys.ENTER)
    select1_elem.send_keys(Keys.DOWN)
    select1_elem.send_keys(Keys.ENTER)
    
#    select1.select_by_visible_text("EMBASSY OF SWEDEN IN TEL AVIV")
    select1_elem.send_keys(Keys.TAB)

    # Wait for the page to reload/populate second combo box
    def viseringstyp_has_options(driver):
      select2_elem = driver.find_element(By.ID, "viseringstyp")
      return len(select2_elem.find_elements(By.TAG_NAME, "option")) > 1

    for i in range(100):
      try:
        select1_elem = wait.until(EC.presence_of_element_located((By.ID, "mottagningsenhet")))
        select1 = Select(select1_elem)
        try:
          select1.select_by_visible_text("EMBASSY OF SWEDEN IN TEL AVIV")
        except Exception as inner_e:
#          print(f"Retry {i}: Exception during embassy selection: {inner_e}")
          pass
      except Exception as e:
        print(f"Retry {i}: Could not reacquire embassy select: {e}")
    
      try:
        if viseringstyp_has_options(driver):
          break
      except Exception as e:
        print(f"Retry {i}: Exception during viseringstyp check: {e}")
    
      time.sleep(0.5)
    
    wait.until(viseringstyp_has_options)
    print('Choosing "viseringstyp" to "apply for Swedish passport or id document" (value=6)"')
    while True:
      try:
        select2_elem = driver.find_element(By.ID, "viseringstyp")
        select2 = Select(select2_elem)
        break
      except Exception as e:
        pass

    # Choose last option by keys
    options = select2.options
    select2_elem.send_keys(Keys.ENTER)
    select2_elem.send_keys(Keys.END)
    for i in range(3):
      select2_elem.send_keys(Keys.UP)
    select2_elem.send_keys(Keys.ENTER)

    select1_elem = wait.until(EC.presence_of_element_located((By.ID, "mottagningsenhet")))
    select1 = Select(select1_elem)
    verify_not_empty(select1, "Embassy/consulate")
    select2_elem = driver.find_element(By.ID, "viseringstyp")
    select2 = Select(select2_elem)
    verify_not_empty(select2, "Type of application")

    # Wait for the page to update appointments info
    time.sleep(3)
    page_source = driver.page_source

    slots_available = "No bookable slots found" not in page_source
    if slots_available:
      res = f"<b>Bookable slots ARE AVAILABLE</b> ☺!\n\nBook here: {BOOK_URL}"
    else:
      res = f"<b>No bookable slots found</b> ☹"
    res = 'Result of Tel Aviv slot check: ' + res
    print(res)

    # TBD - Only send telegram message if slots are available
    asyncio.run(report_result_to_telegram(dov_user_id,
                                          res))
  except Exception as e:
    print(f"Error: {e}")
  finally:
    driver.quit()

if __name__ == "__main__":
  main()
