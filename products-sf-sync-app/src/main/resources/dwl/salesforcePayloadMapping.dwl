%dw 2.0
output application/java 

fun SA1(Att1: String, inputPayload: Object) = do {
    var str = (inputPayload[Att1] as String ++ "″") default ""
    ---
    str
}

fun DA1(Att1: String, Att2: String, inputPayload: Object) = do{
    var arr = [trim((inputPayload[Att1] splitBy "-")[1])] ++ [inputPayload[Att2]]
    ---
    arr
} 

fun DA2(Att1: String, Att2: String, inputPayload: Object) = do {
    var A = Att1 ++ "  " ++ Att2
    var str = (inputPayload[Att1] as String ++ " (" ++ inputPayload[Att2] as String ++ "″)") default ""
    ---
    str
}

fun DA3(Att1: String, Att2: String, inputPayload: Object) = do {
    var A = Att1 ++ "  " ++ Att2
    var str = (inputPayload[Att1] as String ++ "″ x " ++ inputPayload[Att2] as String ++ "″")  default ""
    ---
    str
}

fun DA4(Att1: String, Att2: String, inputPayload: Object) =do {
    var str = (inputPayload[Att1] as String ++ "″ " ++ inputPayload[Att2] as String) default ""
    ---
    str
}

fun MA1(Att: Array, inputPayload: Object) = do{
    var res = (Att map(inputPayload[$]))
    ---
    (res)
}

fun MA2(Att: Any, inputPayload: Object) = do{
    var res = (Att map(inputPayload[$]))
    ---
    ((res -- [null]) -- [""]) joinBy " + "
}

fun MA3(Att1: String, Att2: String, Att3: String, inputPayload: Object) = do{
    var str = (inputPayload[Att1] as String ++ "″ x " ++ inputPayload[Att2] as String ++ "″ " ++ inputPayload[Att3] as String) default ""
    ---
    str
}

fun CR1(Att: String, inputPayload: Object) = do{
    var res = (inputPayload[Att] as String)
    ---
    ((res splitBy ",") map trim($)) joinBy "\n"
}

fun makeDescription(data: Object) = do{
    var descJSON = if(data.IMS_UserDef1 != null) read(data.IMS_UserDef1, "application/json")
                    else ({})
    ---
    (descJSON mapObject ((value, key, index) -> (
    if(value.function != null)(
        (key): value.function match {
            case "DA1" -> DA1(value.Attributes[0], value.Attributes[1], data)
            case "DA2" -> DA2(value.Attributes[0], value.Attributes[1], data)
            case "DA3" -> DA3(value.Attributes[0], value.Attributes[1], data)
            case "DA4" -> DA4(value.Attributes[0], value.Attributes[1], data)
            case "SA1" -> SA1(value.Attributes[0], data)
            case "MA1" -> MA1(value.Attributes, data)
            case "MA2" -> MA2(value.Attributes, data)
            case "CR1" -> CR1(value.Attributes[0], data)
            case "MA3" -> MA3(value.Attributes[0], value.Attributes[1], value.Attributes[2], data)
            else -> "Ivalid Input"
        }
    )
    else (
        (key): data[value.Attributes[0]]
    )
)) mapObject(
    if(typeOf($) as String == "Array")(($$): (($ -- [null]) -- [""]) joinBy ", ")
    else (($$): $)
)) filterObject ((v, k, ind) -> v != "" and v != null)
}

fun descriptionString(data: Object) = do{
    var keys = keysOf(data)
    var values = valuesOf(data)
    ---
    if(sizeOf(keys) > 0)(keys reduce ((item, str = "") -> str ++ (item ++ ": " ++ data[item] ++ "\n")))
    else ("")
}
---
payload map {
	//Recent additions in product sync (9th of July, 2024)
	"IsActive": if($.IMA_ItemStatusCode == "Active") true else false,
	"ProductCode": $.IMA_ItemID,
	
    //Item Info
    "Name": $.IMA_ItemName,                 //Revisit  --> Done
    "ItemID__c": $.IMA_ItemID, 
    "SupercededItemID__c": $.IMA_SupercededItemID,
    "ItemName__c": $.IMA_ItemName,
    "Description": descriptionString(makeDescription($)),
	"Notes__c": $.IMS_LongDesc,
	//  "Classification__c": $.IMA_Classification,      //Revisit
    "Product_Category__c": $.IMA_Classification,      //Revisit
    "UnitMeasure__c": $.IMA_UnitMeasure,
    "Price__c": $.IMA_Price,
    "PriceMatrixFlag__c": $.IMA_PriceMatrixFlag,
    "PriceCodeID__c": $.IMA_PriceCodeID,
    "SalesTaxFlag__c": $.IMA_SalesTaxFlag,
    "Item_Type_Code__c": $.IMA_ItemTypeCode,
    "Customer_Product_ID__c": $.IMA_CustItemID,
    "Rev_Level__c": $.IMA_RevLevel,
    "OnHandQty__c": $.IMA_OnHandQty,
    "Family": $.IMA_ProdFam, 
    "Product_Model__c": $.IMA_ProdModel, 
    // "LeadTimeCode__c": $.IMA_LeadTimeCode,            //Revisit
    "PurLeadTime__c": $.IMA_PurLeadTime,
    "MfgLeadTime__c": $.IMA_MfgLeadTime,
    "ItemStatusCode__c": $.IMA_ItemStatusCode,
    "SalesConvFactor__c": $.IMA_SalesConvFactor,
    // "RoutHrsExcptFlag__c": $.IMA_RoutHrsExcptFlag,    //Revisit
    "GLSalesAcctNbr__c": $.IMA_GLSalesAcctNbr,
    "ERP_Customer_ID__c": $.IMA_CustomerID,
    // "OutplantLeadTime__c": $.IMA_OutplantLeadTime,       // Revisit
    "LastModifiedDate__c": $.IMA_LastModifiedDate as DateTime,
    "Min_Sales_Quantity__c": $.IMA_MinSalesQuantity,
    "SBQQ__NonDiscountable__c": $.IMA_NonDiscountable,
    "Non_Commissionable__c": $.IMA_NonCommisionable,
    "Length__c": $.IMA_Length,
    "Length_Unit_of_Measure__c": $.IMA_LengthUnitMeasure,
    "Width__c": $.IMA_Width,
    "Width_Unit_of_Measure__c": $.IMA_WidthUnitMeasure,
    "Height_Length__c": $.IMA_Height,
    "Height_Length_Unit_of_Measure__c": $.IMA_HeightUnitMeasure,
	"Weight__c": $.IMA_Weight,
    "Weight_Unit_of_Measure__c": $.IMA_WeightUnitMeasure,
	                                 //Add target Salesforce Field
	"Patent_Note__c": $.IMA_UserDef2,																		
	"UserDef__c": if($.IMA_UserDef4 == "1") (true) else (false),	
	"Sales_Description__c": $.IMA_SalesDescription,	
                                     //Attributes
    "Decor__c": $.Attribute1_Value,
    "Decor_2__c": $.Attribute2_Value,
    "Decor_3__c": $.Attribute3_Value,
    "D_cor_1_Direction__c": $.Attribute4_Value,
    "Resin_Type__c": $.Attribute5_Value,
    "Gauge__c": $.Attribute6_Value,
    "Width__c": $.Attribute7_Value,
    "Height_Length__c": $.Attribute8_Value,
    "Front_Finish__c": $.Attribute9_Value,
    "Back_Finish__c": $.Attribute10_Value,
    "Front_Add_on_1__c": $.Attribute11_Value,
    "Front_Add_on_2__c": $.Attribute12_Value,
    "Front_Add_on_3__c": $.Attribute13_Value,
    "Back_Add_on_1__c": $.Attribute14_Value,
    "Back_Add_on_2__c": $.Attribute15_Value,
    "Back_Add_on_3__c": $.Attribute16_Value,
	"Back_Add_On_4__c": $.Attribute17_Value,
    "Price_Group__c": $.Attribute18_Value,
    "Customer_Type__c": $.Attribute19_Value,
    "Sheet_Style__c": $.Attribute20_Value,
    "Kuvio_Tiles_Finish__c": $.Attribute21_Value,
    "Standard_Solution_Type__c": $.Attribute22_Value,
    "Custom_Solution_Type__c": $.Attribute23_Value,
    "LL_Color_Temp__c": $.Attribute24_Value,
    // "": $.Attribute25_Value,
    // "": $.Attribute26_Value,
    // "": $.Attribute27_Value,
    "Edge_Finish__c": $.Attribute28_Value, 
    "Edge_Profile__c": $.Attribute29_Value,
    "Price_Basis__c": $.Attribute30_Value,
	// "Description2__c": descriptionString(makeDescription($))
}