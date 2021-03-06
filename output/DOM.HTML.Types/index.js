// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var DOM_Event_Types = require("../DOM.Event.Types");
var DOM_Node_Types = require("../DOM.Node.Types");
var Data_Either = require("../Data.Either");
var Data_Foreign = require("../Data.Foreign");
var Data_Identity = require("../Data.Identity");
var Data_List_Types = require("../Data.List.Types");
var Prelude = require("../Prelude");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var windowToEventTarget = Unsafe_Coerce.unsafeCoerce;
var readHTMLVideoElement = Data_Foreign.unsafeReadTagged("HTMLVideoElement");
var readHTMLUListElement = Data_Foreign.unsafeReadTagged("HTMLUListElement");
var readHTMLTrackElement = Data_Foreign.unsafeReadTagged("HTMLTrackElement");
var readHTMLTitleElement = Data_Foreign.unsafeReadTagged("HTMLTitleElement");
var readHTMLTimeElement = Data_Foreign.unsafeReadTagged("HTMLTimeElement");
var readHTMLTextAreaElement = Data_Foreign.unsafeReadTagged("HTMLTextAreaElement");
var readHTMLTemplateElement = Data_Foreign.unsafeReadTagged("HTMLTemplateElement");
var readHTMLTableSectionElement = Data_Foreign.unsafeReadTagged("HTMLTableSectionElement");
var readHTMLTableRowElement = Data_Foreign.unsafeReadTagged("HTMLTableRowElement");
var readHTMLTableHeaderCellElement = Data_Foreign.unsafeReadTagged("HTMLTableHeaderCellElement");
var readHTMLTableElement = Data_Foreign.unsafeReadTagged("HTMLTableElement");
var readHTMLTableDataCellElement = Data_Foreign.unsafeReadTagged("HTMLTableDataCellElement");
var readHTMLTableColElement = Data_Foreign.unsafeReadTagged("HTMLTableColElement");
var readHTMLTableCellElement = Data_Foreign.unsafeReadTagged("HTMLTableCellElement");
var readHTMLTableCaptionElement = Data_Foreign.unsafeReadTagged("HTMLTableCaptionElement");
var readHTMLStyleElement = Data_Foreign.unsafeReadTagged("HTMLStyleElement");
var readHTMLSpanElement = Data_Foreign.unsafeReadTagged("HTMLSpanElement");
var readHTMLSourceElement = Data_Foreign.unsafeReadTagged("HTMLSourceElement");
var readHTMLSelectElement = Data_Foreign.unsafeReadTagged("HTMLSelectElement");
var readHTMLScriptElement = Data_Foreign.unsafeReadTagged("HTMLScriptElement");
var readHTMLQuoteElement = Data_Foreign.unsafeReadTagged("HTMLQuoteElement");
var readHTMLProgressElement = Data_Foreign.unsafeReadTagged("HTMLProgressElement");
var readHTMLPreElement = Data_Foreign.unsafeReadTagged("HTMLPreElement");
var readHTMLParamElement = Data_Foreign.unsafeReadTagged("HTMLParamElement");
var readHTMLParagraphElement = Data_Foreign.unsafeReadTagged("HTMLParagraphElement");
var readHTMLOutputElement = Data_Foreign.unsafeReadTagged("HTMLOutputElement");
var readHTMLOptionElement = Data_Foreign.unsafeReadTagged("HTMLOptionElement");
var readHTMLOptGroupElement = Data_Foreign.unsafeReadTagged("HTMLOptGroupElement");
var readHTMLObjectElement = Data_Foreign.unsafeReadTagged("HTMLObjectElement");
var readHTMLOListElement = Data_Foreign.unsafeReadTagged("HTMLOListElement");
var readHTMLModElement = Data_Foreign.unsafeReadTagged("HTMLModElement");
var readHTMLMeterElement = Data_Foreign.unsafeReadTagged("HTMLMeterElement");
var readHTMLMetaElement = Data_Foreign.unsafeReadTagged("HTMLMetaElement");
var readHTMLMediaElement = Data_Foreign.unsafeReadTagged("HTMLMediaElement");
var readHTMLMapElement = Data_Foreign.unsafeReadTagged("HTMLMapElement");
var readHTMLLinkElement = Data_Foreign.unsafeReadTagged("HTMLLinkElement");
var readHTMLLegendElement = Data_Foreign.unsafeReadTagged("HTMLLegendElement");
var readHTMLLabelElement = Data_Foreign.unsafeReadTagged("HTMLLabelElement");
var readHTMLLIElement = Data_Foreign.unsafeReadTagged("HTMLLIElement");
var readHTMLKeygenElement = Data_Foreign.unsafeReadTagged("HTMLKeygenElement");
var readHTMLInputElement = Data_Foreign.unsafeReadTagged("HTMLInputElement");
var readHTMLImageElement = Data_Foreign.unsafeReadTagged("HTMLImageElement");
var readHTMLIFrameElement = Data_Foreign.unsafeReadTagged("HTMLIFrameElement");
var readHTMLHtmlElement = Data_Foreign.unsafeReadTagged("HTMLHtmlElement");
var readHTMLHeadingElement = Data_Foreign.unsafeReadTagged("HTMLHeadingElement");
var readHTMLHeadElement = Data_Foreign.unsafeReadTagged("HTMLHeadElement");
var readHTMLHRElement = Data_Foreign.unsafeReadTagged("HTMLHRElement");
var readHTMLFormElement = Data_Foreign.unsafeReadTagged("HTMLFormElement");
var readHTMLFieldSetElement = Data_Foreign.unsafeReadTagged("HTMLFieldSetElement");
var readHTMLEmbedElement = Data_Foreign.unsafeReadTagged("HTMLEmbedElement");
var readHTMLElement = $foreign._readHTMLElement(function ($0) {
    return Control_Monad_Except_Trans.except(Data_Identity.applicativeIdentity)(Data_Either.Left.create(Control_Applicative.pure(Data_List_Types.applicativeNonEmptyList)(Data_Foreign.TypeMismatch.create("HTMLElement")($0))));
})(function ($1) {
    return Control_Monad_Except_Trans.except(Data_Identity.applicativeIdentity)(Data_Either.Right.create($1));
});
var readHTMLDocument = Data_Foreign.unsafeReadTagged("HTMLDocument");
var readHTMLDivElement = Data_Foreign.unsafeReadTagged("HTMLDivElement");
var readHTMLDataListElement = Data_Foreign.unsafeReadTagged("HTMLDataListElement");
var readHTMLDataElement = Data_Foreign.unsafeReadTagged("HTMLDataElement");
var readHTMLDListElement = Data_Foreign.unsafeReadTagged("HTMLDListElement");
var readHTMLCanvasElement = Data_Foreign.unsafeReadTagged("HTMLCanvasElement");
var readHTMLButtonElement = Data_Foreign.unsafeReadTagged("HTMLButtonElement");
var readHTMLBodyElement = Data_Foreign.unsafeReadTagged("HTMLBodyElement");
var readHTMLBaseElement = Data_Foreign.unsafeReadTagged("HTMLBaseElement");
var readHTMLBRElement = Data_Foreign.unsafeReadTagged("HTMLBRElement");
var readHTMLAudioElement = Data_Foreign.unsafeReadTagged("HTMLAudioElement");
var readHTMLAreaElement = Data_Foreign.unsafeReadTagged("HTMLAreaElement");
var readHTMLAnchorElement = Data_Foreign.unsafeReadTagged("HTMLAnchorElement");
var htmlVideoElementToHTMLMediaElement = Unsafe_Coerce.unsafeCoerce;
var htmlUListElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTrackElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTitleElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTimeElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTextAreaElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTemplateElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableSectionElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableRowElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableHeaderCellElementToHTMLTableCellElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableDataCellElementToHTMLTableCellElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableColElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableCellElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlTableCaptionElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlStyleElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlSpanElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlSourceElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlSelectElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlScriptElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlQuoteElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlProgressElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlPreElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlParamElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlParagraphElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlOutputElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlOptionElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlOptGroupElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlObjectElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlOListElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlModElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlMeterElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlMetaElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlMediaElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlMapElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlLinkElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlLegendElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlLabelElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlLIElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlKeygenElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlInputElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlImageElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlIFrameElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlHtmlElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlHeadingElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlHeadElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlHRElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlFormElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlFieldSetElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlEmbedElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlElementToParentNode = Unsafe_Coerce.unsafeCoerce;
var htmlElementToNonDocumentTypeChildNode = Unsafe_Coerce.unsafeCoerce;
var htmlElementToNode = Unsafe_Coerce.unsafeCoerce;
var htmlElementToEventTarget = Unsafe_Coerce.unsafeCoerce;
var htmlElementToElement = Unsafe_Coerce.unsafeCoerce;
var htmlDocumentToParentNode = Unsafe_Coerce.unsafeCoerce;
var htmlDocumentToNonElementParentNode = Unsafe_Coerce.unsafeCoerce;
var htmlDocumentToNode = Unsafe_Coerce.unsafeCoerce;
var htmlDocumentToEventTarget = Unsafe_Coerce.unsafeCoerce;
var htmlDocumentToDocument = Unsafe_Coerce.unsafeCoerce;
var htmlDivElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlDataListElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlDataElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlDListElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlCanvasElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlButtonElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlBodyElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlBaseElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlBRElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlAudioElementToHTMLMediaElement = Unsafe_Coerce.unsafeCoerce;
var htmlAreaElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
var htmlAnchorElementToHTMLElement = Unsafe_Coerce.unsafeCoerce;
module.exports = {
    htmlAnchorElementToHTMLElement: htmlAnchorElementToHTMLElement, 
    htmlAreaElementToHTMLElement: htmlAreaElementToHTMLElement, 
    htmlAudioElementToHTMLMediaElement: htmlAudioElementToHTMLMediaElement, 
    htmlBRElementToHTMLElement: htmlBRElementToHTMLElement, 
    htmlBaseElementToHTMLElement: htmlBaseElementToHTMLElement, 
    htmlBodyElementToHTMLElement: htmlBodyElementToHTMLElement, 
    htmlButtonElementToHTMLElement: htmlButtonElementToHTMLElement, 
    htmlCanvasElementToHTMLElement: htmlCanvasElementToHTMLElement, 
    htmlDListElementToHTMLElement: htmlDListElementToHTMLElement, 
    htmlDataElementToHTMLElement: htmlDataElementToHTMLElement, 
    htmlDataListElementToHTMLElement: htmlDataListElementToHTMLElement, 
    htmlDivElementToHTMLElement: htmlDivElementToHTMLElement, 
    htmlDocumentToDocument: htmlDocumentToDocument, 
    htmlDocumentToEventTarget: htmlDocumentToEventTarget, 
    htmlDocumentToNode: htmlDocumentToNode, 
    htmlDocumentToNonElementParentNode: htmlDocumentToNonElementParentNode, 
    htmlDocumentToParentNode: htmlDocumentToParentNode, 
    htmlElementToElement: htmlElementToElement, 
    htmlElementToEventTarget: htmlElementToEventTarget, 
    htmlElementToNode: htmlElementToNode, 
    htmlElementToNonDocumentTypeChildNode: htmlElementToNonDocumentTypeChildNode, 
    htmlElementToParentNode: htmlElementToParentNode, 
    htmlEmbedElementToHTMLElement: htmlEmbedElementToHTMLElement, 
    htmlFieldSetElementToHTMLElement: htmlFieldSetElementToHTMLElement, 
    htmlFormElementToHTMLElement: htmlFormElementToHTMLElement, 
    htmlHRElementToHTMLElement: htmlHRElementToHTMLElement, 
    htmlHeadElementToHTMLElement: htmlHeadElementToHTMLElement, 
    htmlHeadingElementToHTMLElement: htmlHeadingElementToHTMLElement, 
    htmlHtmlElementToHTMLElement: htmlHtmlElementToHTMLElement, 
    htmlIFrameElementToHTMLElement: htmlIFrameElementToHTMLElement, 
    htmlImageElementToHTMLElement: htmlImageElementToHTMLElement, 
    htmlInputElementToHTMLElement: htmlInputElementToHTMLElement, 
    htmlKeygenElementToHTMLElement: htmlKeygenElementToHTMLElement, 
    htmlLIElementToHTMLElement: htmlLIElementToHTMLElement, 
    htmlLabelElementToHTMLElement: htmlLabelElementToHTMLElement, 
    htmlLegendElementToHTMLElement: htmlLegendElementToHTMLElement, 
    htmlLinkElementToHTMLElement: htmlLinkElementToHTMLElement, 
    htmlMapElementToHTMLElement: htmlMapElementToHTMLElement, 
    htmlMediaElementToHTMLElement: htmlMediaElementToHTMLElement, 
    htmlMetaElementToHTMLElement: htmlMetaElementToHTMLElement, 
    htmlMeterElementToHTMLElement: htmlMeterElementToHTMLElement, 
    htmlModElementToHTMLElement: htmlModElementToHTMLElement, 
    htmlOListElementToHTMLElement: htmlOListElementToHTMLElement, 
    htmlObjectElementToHTMLElement: htmlObjectElementToHTMLElement, 
    htmlOptGroupElementToHTMLElement: htmlOptGroupElementToHTMLElement, 
    htmlOptionElementToHTMLElement: htmlOptionElementToHTMLElement, 
    htmlOutputElementToHTMLElement: htmlOutputElementToHTMLElement, 
    htmlParagraphElementToHTMLElement: htmlParagraphElementToHTMLElement, 
    htmlParamElementToHTMLElement: htmlParamElementToHTMLElement, 
    htmlPreElementToHTMLElement: htmlPreElementToHTMLElement, 
    htmlProgressElementToHTMLElement: htmlProgressElementToHTMLElement, 
    htmlQuoteElementToHTMLElement: htmlQuoteElementToHTMLElement, 
    htmlScriptElementToHTMLElement: htmlScriptElementToHTMLElement, 
    htmlSelectElementToHTMLElement: htmlSelectElementToHTMLElement, 
    htmlSourceElementToHTMLElement: htmlSourceElementToHTMLElement, 
    htmlSpanElementToHTMLElement: htmlSpanElementToHTMLElement, 
    htmlStyleElementToHTMLElement: htmlStyleElementToHTMLElement, 
    htmlTableCaptionElementToHTMLElement: htmlTableCaptionElementToHTMLElement, 
    htmlTableCellElementToHTMLElement: htmlTableCellElementToHTMLElement, 
    htmlTableColElementToHTMLElement: htmlTableColElementToHTMLElement, 
    htmlTableDataCellElementToHTMLTableCellElement: htmlTableDataCellElementToHTMLTableCellElement, 
    htmlTableElementToHTMLElement: htmlTableElementToHTMLElement, 
    htmlTableHeaderCellElementToHTMLTableCellElement: htmlTableHeaderCellElementToHTMLTableCellElement, 
    htmlTableRowElementToHTMLElement: htmlTableRowElementToHTMLElement, 
    htmlTableSectionElementToHTMLElement: htmlTableSectionElementToHTMLElement, 
    htmlTemplateElementToHTMLElement: htmlTemplateElementToHTMLElement, 
    htmlTextAreaElementToHTMLElement: htmlTextAreaElementToHTMLElement, 
    htmlTimeElementToHTMLElement: htmlTimeElementToHTMLElement, 
    htmlTitleElementToHTMLElement: htmlTitleElementToHTMLElement, 
    htmlTrackElementToHTMLElement: htmlTrackElementToHTMLElement, 
    htmlUListElementToHTMLElement: htmlUListElementToHTMLElement, 
    htmlVideoElementToHTMLMediaElement: htmlVideoElementToHTMLMediaElement, 
    readHTMLAnchorElement: readHTMLAnchorElement, 
    readHTMLAreaElement: readHTMLAreaElement, 
    readHTMLAudioElement: readHTMLAudioElement, 
    readHTMLBRElement: readHTMLBRElement, 
    readHTMLBaseElement: readHTMLBaseElement, 
    readHTMLBodyElement: readHTMLBodyElement, 
    readHTMLButtonElement: readHTMLButtonElement, 
    readHTMLCanvasElement: readHTMLCanvasElement, 
    readHTMLDListElement: readHTMLDListElement, 
    readHTMLDataElement: readHTMLDataElement, 
    readHTMLDataListElement: readHTMLDataListElement, 
    readHTMLDivElement: readHTMLDivElement, 
    readHTMLDocument: readHTMLDocument, 
    readHTMLElement: readHTMLElement, 
    readHTMLEmbedElement: readHTMLEmbedElement, 
    readHTMLFieldSetElement: readHTMLFieldSetElement, 
    readHTMLFormElement: readHTMLFormElement, 
    readHTMLHRElement: readHTMLHRElement, 
    readHTMLHeadElement: readHTMLHeadElement, 
    readHTMLHeadingElement: readHTMLHeadingElement, 
    readHTMLHtmlElement: readHTMLHtmlElement, 
    readHTMLIFrameElement: readHTMLIFrameElement, 
    readHTMLImageElement: readHTMLImageElement, 
    readHTMLInputElement: readHTMLInputElement, 
    readHTMLKeygenElement: readHTMLKeygenElement, 
    readHTMLLIElement: readHTMLLIElement, 
    readHTMLLabelElement: readHTMLLabelElement, 
    readHTMLLegendElement: readHTMLLegendElement, 
    readHTMLLinkElement: readHTMLLinkElement, 
    readHTMLMapElement: readHTMLMapElement, 
    readHTMLMediaElement: readHTMLMediaElement, 
    readHTMLMetaElement: readHTMLMetaElement, 
    readHTMLMeterElement: readHTMLMeterElement, 
    readHTMLModElement: readHTMLModElement, 
    readHTMLOListElement: readHTMLOListElement, 
    readHTMLObjectElement: readHTMLObjectElement, 
    readHTMLOptGroupElement: readHTMLOptGroupElement, 
    readHTMLOptionElement: readHTMLOptionElement, 
    readHTMLOutputElement: readHTMLOutputElement, 
    readHTMLParagraphElement: readHTMLParagraphElement, 
    readHTMLParamElement: readHTMLParamElement, 
    readHTMLPreElement: readHTMLPreElement, 
    readHTMLProgressElement: readHTMLProgressElement, 
    readHTMLQuoteElement: readHTMLQuoteElement, 
    readHTMLScriptElement: readHTMLScriptElement, 
    readHTMLSelectElement: readHTMLSelectElement, 
    readHTMLSourceElement: readHTMLSourceElement, 
    readHTMLSpanElement: readHTMLSpanElement, 
    readHTMLStyleElement: readHTMLStyleElement, 
    readHTMLTableCaptionElement: readHTMLTableCaptionElement, 
    readHTMLTableCellElement: readHTMLTableCellElement, 
    readHTMLTableColElement: readHTMLTableColElement, 
    readHTMLTableDataCellElement: readHTMLTableDataCellElement, 
    readHTMLTableElement: readHTMLTableElement, 
    readHTMLTableHeaderCellElement: readHTMLTableHeaderCellElement, 
    readHTMLTableRowElement: readHTMLTableRowElement, 
    readHTMLTableSectionElement: readHTMLTableSectionElement, 
    readHTMLTemplateElement: readHTMLTemplateElement, 
    readHTMLTextAreaElement: readHTMLTextAreaElement, 
    readHTMLTimeElement: readHTMLTimeElement, 
    readHTMLTitleElement: readHTMLTitleElement, 
    readHTMLTrackElement: readHTMLTrackElement, 
    readHTMLUListElement: readHTMLUListElement, 
    readHTMLVideoElement: readHTMLVideoElement, 
    windowToEventTarget: windowToEventTarget
};
