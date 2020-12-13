package com.gildedrose

class GildedRose(val items: Array[Item]) {


  def updateQuality() {
    for (i <- 0 until items.length) {

      updateItemQuality(items(i))
    }
  }

  def updateItemQuality(item: Item) {
    if (isIncreaseValueWithTime(item)) {
      // special item, increases value with time
      updateQualityForSpecialItem(item)
    } else {
      // standard item, decrease quality with time
      updateQualityForStandardItem(item)
    }

    // update the sell by date, but not for sulfuras (never to be sold)
    if (!isSulfurasItem(item)) {
      item.sellIn = item.sellIn - 1
    }

    // if item is already to be sold
    if (item.sellIn < 0) {
      // update quality for due time items
      updateQualityForDueItem(item)
    }
    ensureQualityIsNotMoreThan50(item)
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

  def isIncreaseValueWithTime(item: Item): Boolean = {
    this.isAgedBrieItem(item) || this.isBackstagePassesItem(item)
  }

  def updateQualityForSpecialItem(item: Item): Unit = {
    // is special item
    item.quality = item.quality + 1
    if (isBackstagePassesItem(item)) {
      val days = item.sellIn
      if (days <= 10) {
        // 10 days before the concert: add one more point
        item.quality = item.quality + 1
      }
      if (days <= 5) {
        // 5 days before the concert: add one more extra point
        item.quality = item.quality + 1
      }
    }
  }

  def updateQualityForStandardItem(item: Item): Unit = {
    // is standard item here, decrease quality
    if (item.quality > 0 && !isSulfurasItem(item)) {
        // is not sulfuras
        item.quality = item.quality - 1
    }
  }

  def updateQualityForDueItem(item: Item): Unit = {
    if (isAgedBrieItem(item)) {
      // it is aged brie
      item.quality = item.quality + 1
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

  def ensureQualityIsNotMoreThan50(item: Item): Unit = {
    // if it is more than 50, return to 50 again
    if (item.quality > 50 && !isSulfurasItem(item)) {
      item.quality = 50
    }
  }
}
