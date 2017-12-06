package service

import org.scalatestplus.play.PlaySpec
import Utils._

class UtilsTest extends PlaySpec {
  "Day 1 Part 1" must {
    "summarize values in string if next is equal" in {
      sumNextIfEqual("1122") mustBe 3
      sumNextIfEqual("1111") mustBe 4
      sumNextIfEqual("1234") mustBe 0
      sumNextIfEqual("91212129") mustBe 9

      val challenge = "29917128875332952564321392569634257121244516819997569284938677239676779378822158323549832814412597817651244117851771257438674567254146559419528411463781241159837576747416543451994579655175322397355255587935456185669334559882554936642122347526466965746273596321419312386992922582836979771421518356285534285825212798113159911272923448284681544657616654285632235958355867722479252256292311384799669645293812691169936746744856227797779513997329663235176153745581296191298956836998758194274865327383988992499115472925731787228592624911829221985925935268785757854569131538763133427434848767475989173579655375125972435359317237712667658828722623837448758528395981635746922144957695238318954845799697142491972626942976788997427135797297649149849739186827185775786254552866371729489943881272817466129271912247236569141713377483469323737384967871876982476485658337183881519295728697121462266226452265259877781881868585356333494916519693683238733823362353424927852348119426673294798416314637799636344448941782774113142925315947664869341363354235389597893211532745789957591898692253157726576488811769461354938575527273474399545366389515353657644736458182565245181653996192644851687269744491856672563885457872883368415631469696994757636288575816146927747179133188841148212825453859269643736199836818121559198563122442483528316837885842696283932779475955796132242682934853291737434482287486978566652161245555856779844813283979453489221189332412315117573259531352875384444264457373153263878999332444178577127433891164266387721116357278222665798584824336957648454426665495982221179382794158366894875864761266695773155813823291684611617853255857774422185987921219618596814446229556938354417164971795294741898631698578989231245376826359179266783767935932788845143542293569863998773276365886375624694329228686284863341465994571635379257258559894197638117333711626435669415976255967412994139131385751822134927578932521461677534945328228131973291962134523589491173343648964449149716696761218423314765168285342711137126239639867897341514131244859826663281981251614843274762372382114258543828157464392"
      sumNextIfEqual(challenge) mustBe 1141
    }
  }

  "Day 1 Part 2" must {
    "sum if half between list is equal" in {
      sumIfHalfWayEqual("1212") mustBe 6
      sumIfHalfWayEqual("1221") mustBe 0
      sumIfHalfWayEqual("123425") mustBe 4
      sumIfHalfWayEqual("123123") mustBe 12
      sumIfHalfWayEqual("12131415") mustBe 4

      val challenge = "29917128875332952564321392569634257121244516819997569284938677239676779378822158323549832814412597817651244117851771257438674567254146559419528411463781241159837576747416543451994579655175322397355255587935456185669334559882554936642122347526466965746273596321419312386992922582836979771421518356285534285825212798113159911272923448284681544657616654285632235958355867722479252256292311384799669645293812691169936746744856227797779513997329663235176153745581296191298956836998758194274865327383988992499115472925731787228592624911829221985925935268785757854569131538763133427434848767475989173579655375125972435359317237712667658828722623837448758528395981635746922144957695238318954845799697142491972626942976788997427135797297649149849739186827185775786254552866371729489943881272817466129271912247236569141713377483469323737384967871876982476485658337183881519295728697121462266226452265259877781881868585356333494916519693683238733823362353424927852348119426673294798416314637799636344448941782774113142925315947664869341363354235389597893211532745789957591898692253157726576488811769461354938575527273474399545366389515353657644736458182565245181653996192644851687269744491856672563885457872883368415631469696994757636288575816146927747179133188841148212825453859269643736199836818121559198563122442483528316837885842696283932779475955796132242682934853291737434482287486978566652161245555856779844813283979453489221189332412315117573259531352875384444264457373153263878999332444178577127433891164266387721116357278222665798584824336957648454426665495982221179382794158366894875864761266695773155813823291684611617853255857774422185987921219618596814446229556938354417164971795294741898631698578989231245376826359179266783767935932788845143542293569863998773276365886375624694329228686284863341465994571635379257258559894197638117333711626435669415976255967412994139131385751822134927578932521461677534945328228131973291962134523589491173343648964449149716696761218423314765168285342711137126239639867897341514131244859826663281981251614843274762372382114258543828157464392"
      sumIfHalfWayEqual(challenge) mustBe 950
    }
  }

  "Day 2 Part 1" must {
    "calculate diff per row" in {
      calcRowDiff("5 1 9 5") mustBe 8
      calcRowDiff("7 5 3") mustBe 4
      calcRowDiff("2 4 6 8") mustBe 6
    }
    "calculate checksum from rows" in {
      val input = "5 1 9 5\n7 5 3\n2 4 6 8"
      calcChecksum(input) mustBe 18

      val challenge = "157\t564\t120\t495\t194\t520\t510\t618\t244\t443\t471\t473\t612\t149\t506\t138\n1469\t670\t47\t604\t1500\t238\t1304\t1426\t54\t749\t1218\t1409\t60\t51\t1436\t598\n578\t184\t2760\t3057\t994\t167\t2149\t191\t2913\t2404\t213\t1025\t1815\t588\t2421\t3138\n935\t850\t726\t155\t178\t170\t275\t791\t1028\t75\t781\t138\t176\t621\t773\t688\n212\t977\t297\t645\t229\t194\t207\t640\t804\t509\t833\t726\t197\t825\t242\t743\n131\t43\t324\t319\t64\t376\t231\t146\t382\t162\t464\t314\t178\t353\t123\t446\n551\t121\t127\t155\t1197\t288\t1412\t1285\t557\t137\t145\t1651\t1549\t1217\t681\t1649\n1723\t1789\t5525\t4890\t3368\t188\t3369\t4842\t3259\t2502\t4825\t163\t146\t2941\t126\t5594\n311\t2420\t185\t211\t2659\t2568\t2461\t231\t2599\t1369\t821\t506\t2227\t180\t220\t1372\n197\t4490\t141\t249\t3615\t3314\t789\t4407\t169\t352\t4383\t5070\t5173\t3115\t132\t3513\n4228\t2875\t3717\t504\t114\t2679\t165\t3568\t3002\t116\t756\t151\t4027\t261\t4813\t2760\n651\t3194\t2975\t2591\t1019\t835\t3007\t248\t3028\t1382\t282\t3242\t296\t270\t3224\t3304\n1858\t1650\t1720\t1848\t95\t313\t500\t1776\t207\t1186\t72\t259\t281\t1620\t79\t77\n3841\t3217\t440\t3481\t3643\t940\t3794\t4536\t1994\t4040\t3527\t202\t193\t1961\t230\t217\n2837\t2747\t2856\t426\t72\t78\t2361\t96\t2784\t2780\t98\t2041\t2444\t1267\t2167\t2480\n411\t178\t4263\t4690\t3653\t162\t3201\t4702\t3129\t2685\t3716\t147\t3790\t4888\t79\t165"
      calcChecksum(challenge) mustBe 43074
    }
  }

  "Day 2 Part 2" must {
    "calculate diff per row" in {
      calcRowDivide("5 9 2 8") mustBe 4
      calcRowDivide("9 4 7 3") mustBe 3
      calcRowDivide("3 8 6 5") mustBe 2
    }
    "calculate checksum from rows" in {
      val input = "5 9 2 8\n9 4 7 3\n3 8 6 5"
      calcChecksum2(input) mustBe 9

      val challenge = "157\t564\t120\t495\t194\t520\t510\t618\t244\t443\t471\t473\t612\t149\t506\t138\n1469\t670\t47\t604\t1500\t238\t1304\t1426\t54\t749\t1218\t1409\t60\t51\t1436\t598\n578\t184\t2760\t3057\t994\t167\t2149\t191\t2913\t2404\t213\t1025\t1815\t588\t2421\t3138\n935\t850\t726\t155\t178\t170\t275\t791\t1028\t75\t781\t138\t176\t621\t773\t688\n212\t977\t297\t645\t229\t194\t207\t640\t804\t509\t833\t726\t197\t825\t242\t743\n131\t43\t324\t319\t64\t376\t231\t146\t382\t162\t464\t314\t178\t353\t123\t446\n551\t121\t127\t155\t1197\t288\t1412\t1285\t557\t137\t145\t1651\t1549\t1217\t681\t1649\n1723\t1789\t5525\t4890\t3368\t188\t3369\t4842\t3259\t2502\t4825\t163\t146\t2941\t126\t5594\n311\t2420\t185\t211\t2659\t2568\t2461\t231\t2599\t1369\t821\t506\t2227\t180\t220\t1372\n197\t4490\t141\t249\t3615\t3314\t789\t4407\t169\t352\t4383\t5070\t5173\t3115\t132\t3513\n4228\t2875\t3717\t504\t114\t2679\t165\t3568\t3002\t116\t756\t151\t4027\t261\t4813\t2760\n651\t3194\t2975\t2591\t1019\t835\t3007\t248\t3028\t1382\t282\t3242\t296\t270\t3224\t3304\n1858\t1650\t1720\t1848\t95\t313\t500\t1776\t207\t1186\t72\t259\t281\t1620\t79\t77\n3841\t3217\t440\t3481\t3643\t940\t3794\t4536\t1994\t4040\t3527\t202\t193\t1961\t230\t217\n2837\t2747\t2856\t426\t72\t78\t2361\t96\t2784\t2780\t98\t2041\t2444\t1267\t2167\t2480\n411\t178\t4263\t4690\t3653\t162\t3201\t4702\t3129\t2685\t3716\t147\t3790\t4888\t79\t165"
      calcChecksum2(challenge) mustBe 280
    }
  }

  "Day 3 part 1" must {
    "calculate proper position for an integer" in {
      getPosition(1) mustBe Pos(0, 0)
      getPosition(2) mustBe Pos(1, 0)
      getPosition(3) mustBe Pos(1, 1)
      getPosition(4) mustBe Pos(0, 1)
      getPosition(5) mustBe Pos(-1, 1)
      getPosition(6) mustBe Pos(-1, 0)
      getPosition(10) mustBe Pos(2, -1)
      getPosition(12) mustBe Pos(2, 1)
      getPosition(23) mustBe Pos(0, -2)
    }

    "calculate distance for a position to 1" in {
      getDistance(getPosition(1)) mustBe 0
      getDistance(getPosition(12)) mustBe 3
      getDistance(getPosition(23)) mustBe 2
      getDistance(getPosition(1024)) mustBe 31
      getDistance(getPosition(361527)) mustBe 326
    }
  }

  "Day 3 part 2" must {
    "get all smaller neighbors" in {
      getNeighbors(1) mustBe List.empty
      getNeighbors(2) mustBe List(Pos(0, 0))
      getNeighbors(3) mustBe List(Pos(0,0), Pos(1, 0))
      getNeighbors(4) mustBe List(Pos(0,0), Pos(1, 0), Pos(1, 1))
      getNeighbors(5) mustBe List(Pos(0,0), Pos(0, 1))
    }
    "get sum of adjacent squares" in {
      getSumofAdjacent(1) mustBe 1
      getSumofAdjacent(2) mustBe 1
      getSumofAdjacent(3) mustBe 2
      getSumofAdjacent(4) mustBe 4
      getSumofAdjacent(5) mustBe 5
      getSumofAdjacent(10) mustBe 26
    }
    "get position larger than" in {
      getFirstLargerThan(26) mustBe 54
      getFirstLargerThan(361527) mustBe 363010
    }
  }
}
