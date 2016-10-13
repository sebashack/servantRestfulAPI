{-# LANGUAGE OverloadedStrings     #-}


module Schemas.ElasticSearch.BookingMappings
       (
         propertyMapping,
         bookableMapping
       ) where

import qualified Data.Text as T
import HelperLibs.ElasticSearch.BodyBuilder

{-
There are some fields in the following mappings that will work as filters. The most important to have into
account is the "(prop_)country_code" field. If a client wants to look for bookables, it will always have to specify the
country ISO 3166 alpha-2 country code. Thus, documents which do not exactly match the country will be left out. This is
important when interpreting the pricings of a bookable. All Bookable's pricings will implicitly have a price in the currency
of the country where that bookable belongs to.
-}



-- Property Mapping

regDate = objStr ("reg_date", [strVal ("type", "date"),
                               strVal ("format", "strict_date"),
                               strVal ("index", "not_analyzed")])
adminId = objStr ("admin_id", [strVal ("type", "long")])
propName = objStr ("prop_name", [strVal ("type", "string"), strVal ("analyzer", "locAnalyzer")])
lodgingType = objStr ("lodging_type", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
espPropdesc = objStr ("esp_prop_desc", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
engPropdesc = objStr ("eng_prop_desc", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
countryCode = objStr ("country_code", [strVal ("type", "string"), strVal ("index", "not_analyzed")]) -- country_code filter.
region = objStr ("region", [strVal ("type", "string"), strVal ("analyzer", "locAnalyzer")])
city = objStr ("city", [strVal ("type", "string"), strVal ("analyzer", "locAnalyzer")])
address = objStr ("address", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
zipCode = objStr ("zip_code", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
facilities = objStr ("facilities", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
contactNums = objStr ("contact_nums", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
rules = objStr ("rules", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
mainImgId = objStr ("main_img_id", [strVal ("type", "string"), strVal ("index", "not_analyzed")])


propertyMapping' = objStr("properties", [regDate,
                                         adminId,
                                         propName,
                                         lodgingType,
                                         espPropdesc,
                                         engPropdesc,
                                         countryCode,
                                         region,
                                         city,
                                         address,
                                         zipCode,
                                         facilities,
                                         contactNums,
                                         rules,
                                         mainImgId])


propertyMapping :: (T.Text, T.Text)
propertyMapping = ("api_property", propertyMapping')




-- Bookable Mapping

{-
The Bookable Mapping has some replicated fields from the Property Mapping, namely: prop_id, prop_type, prop_country, prop_region,
prop_city, prop_facilities.

The reasons for this replication are the following:

- It wouldn't be convinient at all to have a bookable as a nested data type inside a property: it could lead to unexpected results
  when searching.

- Bookables can have search criteria based on the property's country, region, city, type and facilities.

- A property has a very small amount of bookables (at most 10), so updating a property will not imply to update a massive
  amount of data.

- Updating a property is no a very frequent operation, specially its location and type.

Thus, when updating a property, you must also update the respective fields in the bookable.

Note that the Domain API for a bookable has to take into account all this even though the Domain DataType does not have the
region, city and propType fields.

-}

propId = objStr ("prop_id", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
propType = objStr ("prop_type", [strVal ("type", "string"), strVal ("index", "not_analyzed")]) 
propCountryCode = objStr ("prop_country_code", [strVal ("type", "string"), strVal ("index", "not_analyzed")]) -- country_code Filter 
propRegion = objStr ("prop_region", [strVal ("type", "string"), strVal ("analyzer", "locAnalyzer")])
propCity = objStr ("prop_city", [strVal ("type", "string"), strVal ("analyzer", "locAnalyzer")])
propFacilities = objStr ("prop_facilities", [strVal ("type", "string"), strVal ("analyzer", "tagAnalyzer")])
bklName = objStr ("bkl_name", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
espBkldesc = objStr ("esp_bkl_desc", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
engBkldesc = objStr ("eng_bkl_desc", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
roomSize = objStr ("room_size", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
bedType = objStr ("bed_type", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
bedNum = objStr ("bed_num", [strVal ("type", "short")])
maxOccu = objStr ("max_occupancy", [strVal ("type", "short")])
amenities = objStr ("amenities", [strVal ("type", "string"), strVal ("analyzer", "tagAnalyzer")])
roomIds = objStr ("room_ids", [strVal ("type", "string"), strVal ("index", "not_analyzed")])
bklState = objStr ("listed", [strVal ("type", "boolean")])
-- Object Type: pricings = [pricing]
pricing = objStr ("properties", [objStr ("pri_id", [strVal ("type", "string"), strVal ("index", "not_analyzed")]),
                                 objStr ("occupancy", [strVal ("type", "short")]),
                                 objStr ("conditions", [strVal ("type", "string"), strVal ("index", "not_analyzed")]),
                                 objStr ("night_price", [strVal ("type", "long")]),
                                 objStr ("discount", [strVal ("type", "short")])])
pricings = objStr ("pricings", [pricing])


bookableMapping' = objStr("properties", [propId,
                                         propType,
                                         propCountryCode,
                                         propRegion,
                                         propCity,
                                         propFacilities,
                                         bklName,
                                         espBkldesc,
                                         engBkldesc,
                                         roomSize,
                                         bedType,
                                         bedNum,
                                         maxOccu,
                                         amenities,
                                         roomIds,
                                         bklState,
                                         pricings])
                                         
bookableMapping :: (T.Text, T.Text)
bookableMapping = ("api_bookable", bookableMapping')

