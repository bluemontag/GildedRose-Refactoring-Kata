package com.gildedrose

class GildedRose(val items: Array[Item]) {


  def updateQuality() {
    for (i <- 0 until items.length) {

      updateItem(items(i))
    }
  }

  def updateItem(item: Item) {
    if (!isAgedBrieItem(item)
      && !isBackstagePassesItem(item)) {
      if (item.quality > 0 && !isSulfurasItem(item)) {
          item.quality = item.quality - 1
      }
    } else {
      if (item.quality < 50) {
        item.quality = item.quality + 1

        if (isBackstagePassesItem(item)) {
          if (item.sellIn < 11 && item.quality < 50) {
              item.quality = item.quality + 1
          }
          if (item.sellIn < 6 && item.quality < 50) {
              item.quality = item.quality + 1
          }
        }
      }
    }

    if (!isSulfurasItem(item)) {
      item.sellIn = item.sellIn - 1
    }

    if (item.sellIn < 0) {
      if (!isAgedBrieItem(item)) {
        if (!isBackstagePassesItem(item)) {
          if (item.quality > 0 && !isSulfurasItem(item)) {
              item.quality = item.quality - 1
          }
        } else {
          item.quality = 0
        }
      } else {
        if (item.quality < 50) {
          item.quality = item.quality + 1
        }
      }
    }
  }

  def isAgedBrieItem(item: Item): Boolean = {
    item.name.equals("Aged Brie")
  }

  def isBackstagePassesItem(item: Item): Boolean = {
    item.name.equals("Backstage passes to a TAFKAL80ETC concert")
  }

  def isSulfurasItem(item: Item): Boolean = {
    item.name.equals("Sulfuras, Hand of Ragnaros")
  }
}