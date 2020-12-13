package com.gildedrose

class GildedRose(val items: Array[Item]) {


  def updateQuality() {
    for (i <- 0 until items.length) {

      updateItemQuality(items(i))
    }
  }

  def updateItemQuality(item: Item) {
    if (isAgedBrieItem(item) || isBackstagePassesItem(item)) {
      // is special item
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
    } else {
      // is standard item here
      if (item.quality > 0 && !isSulfurasItem(item)) {
          // is not sulfuras
          item.quality = item.quality - 1
      }
    }

    // update the sell by date, but not for sulfuras (never to be sold)
    if (!isSulfurasItem(item)) {
      item.sellIn = item.sellIn - 1
    }

    // update quality for sold items
    if (item.sellIn < 0) {
      if (isAgedBrieItem(item)) {
        // it is aged brie
        if (item.quality < 50) {
          item.quality = item.quality + 1
        }
      } else {
        // if it is a concert ticket
        if (isBackstagePassesItem(item)) {
          // ticket no longer valid, quality is zero
          item.quality = 0
        } else {
          // is not a concert pass ticket
          // for any other items except sulfuras
          if (item.quality > 0 && !isSulfurasItem(item)) {
              // quality degrades by 1
              item.quality = item.quality - 1
          }
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