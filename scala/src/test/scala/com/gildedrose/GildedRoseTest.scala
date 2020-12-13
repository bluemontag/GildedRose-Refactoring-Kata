package com.gildedrose

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.compatible.Assertion

class GildedRoseTest extends AnyFlatSpec with Matchers {
    it should "foo" in {
      val items = Array[Item](new Item("foo", 0, 0))
      val app = new GildedRose(items)
      app.updateQuality()
      app.items(0).name should equal ("foo")
    }

    var items = Array[Item]()
    var app = new GildedRose(items)

    def resetItems(): Unit = {
      items = Array[Item](
        new Item("+5 Dexterity Vest", 10, 20),
        new Item("Aged Brie", 2, 0),
        new Item("Elixir of the Mongoose", 5, 7),
        new Item("Sulfuras, Hand of Ragnaros", 0, 80),
        new Item("Sulfuras, Hand of Ragnaros", -1, 80),
        new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
        new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
        new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),
        // this conjured item does not work properly yet
        new Item("Conjured Mana Cake", 3, 6)
      )
      app = new GildedRose(items)
    }

    val dontFilterItems = Option.empty

    "All items" should "have a positive quality value" in {
      this.simulate50Days(dontFilterItems, (item, lastQ) => item.quality should be >= 0)
    }
    
    def simulate50Days(itemName: Option[String], assertion: (Item, Int) => Assertion) {
      this.resetItems()
      for (day <- 1 to 50) {
        // print("Day " + day)
        val filteredItems = if (itemName.isEmpty)
                              app.items // don't filter (assertion must hold for all elements)
                            else 
                              app.items.filter( item => item.name.startsWith(itemName.get))

        // read quality before update
        val lastQualities = filteredItems.map( item => item.quality)
        app.updateQuality()
        for ((item, i)  <- filteredItems.zipWithIndex) {
          assertion(item, lastQualities(i))
        }
        println("Assertion passed for items:\n" + filteredItems.map( item => item.name ).mkString("\n"))
      }
    }

    "All items" should "have not a quality value greater than 50 (except for Sulfuras)" in {
      this.simulate50Days(dontFilterItems, this.qualityIsLessThan50ExceptForSulfuras)
    }

    def qualityIsLessThan50ExceptForSulfuras(item: Item, lastQuality: Int) = {
      if (item.name.startsWith("Sulfuras")) {
        // its special item
        item.quality should equal (80)
      } else {
        // its a normal item
        item.quality should be <= 50
      }
    }

    "Backstage passes" should "increase their value before the concert" in {
      val itemBeginsWithBackstage = Option("Backstage")
      this.simulate50Days(itemBeginsWithBackstage, this.backstagePassesIncreasesValuesBeforeTheConcert)
    }

    def backstagePassesIncreasesValuesBeforeTheConcert(item: Item, lastQuality: Int): Assertion = {
      item.sellIn match {
        case i if (5 <= i && i < 10 && (lastQuality + 2) <= 50) =>
              // when between 5 and 10 days before the concert
              item.quality should equal (lastQuality + 2)
        case i if (0 <= i && i < 5 && (lastQuality + 3) <= 50) =>
              // when between 0 and 5 days before the concert
              item.quality should equal (lastQuality + 3)
        case i if (i < 0) =>
              // when concert has passed, quality is 0
              item.quality should equal (0)
        case _ =>
              // in any case, never more than 50
              item.quality should be <= 50
      }
    }

    "Aged Brie" should "increase its value when it gets older" in {
      val itemBeginsWithAgedBrie = Option("Aged Brie")
      this.simulate50Days(itemBeginsWithAgedBrie, this.itemIncreaseValueWhenOlder)
    }

    def itemIncreaseValueWhenOlder(item: Item, lastQuality: Int): Assertion = {
      if (lastQuality == 50) 
        item.quality should equal (50)
      else 
        item.quality should be > lastQuality
    }

    "Dexterity items" should "degrade quality twice as fast when sell date has passed" in {
      val itemBeginsWithDexterity = Option("+5 Dexterity")
      this.simulate50Days(itemBeginsWithDexterity, this.itemDegradesTwiceAsFastWhenSold)
    }
    
    def itemDegradesTwiceAsFastWhenSold(item: Item, lastQuality: Int): Assertion = {

      val expectedQuality = if (item.sellIn < 0)
                              lastQuality - 2 // sold item
                            else
                              lastQuality - 1 // not sold item

      if (expectedQuality >= 0) {
        item.quality should equal (expectedQuality)
      } else {
        // item reached 0 quality
        item.quality should equal (0)
      }
    }

    "Elixir items" should "degrade quality twice as fast when sell date has passed" in {
      val itemBeginsWithElixir = Option("Elixir")
      this.simulate50Days(itemBeginsWithElixir, this.itemDegradesTwiceAsFastWhenSold)
    }

}
