module Supler {
  export class Form {
    private i18n:I18n;
    private validatorFnFactories:any;
    private validation:Validation;
    private validatorRenderOptions:ValidatorRenderOptions;
    private sendControllerOptions:SendControllerOptions;
    private elementSearch:ElementSearch;
    private renderOptionsGetter:RenderOptionsGetter;
    private afterRenderFn:() => void;
    private customDataHandlerFn:(any) => void;
    private fieldsOptions:FieldsOptions;
    private fieldOrder:string[][];
    private readFormValues:ReadFormValues;

    constructor(private container:HTMLElement, customOptions:any) {
      customOptions = customOptions || {};

      this.fieldsOptions = new FieldsOptions(customOptions.field_options);

      this.i18n = new I18n();
      Util.copyProperties(this.i18n, customOptions.i18n);

      var renderOptions = new Bootstrap3RenderOptions();
      Util.copyProperties(renderOptions, customOptions.render_options);
      this.renderOptionsGetter = RenderOptionsGetter.parse(renderOptions, container, this.fieldsOptions,
        customOptions.field_templates);

      this.validatorFnFactories = new ValidatorFnFactories(this.i18n);
      Util.copyProperties(this.validatorFnFactories, customOptions.validators);

      this.validatorRenderOptions = new ValidatorRenderOptions;
      Util.copyProperties(this.validatorRenderOptions, customOptions.validation_render);

      this.sendControllerOptions = new SendControllerOptions(customOptions);

      this.elementSearch = new ElementSearch(container);

      this.afterRenderFn = customOptions.after_render_function || (() => {
      });
      this.customDataHandlerFn = customOptions.custom_data_handler || ((data:any) => {
      });

      this.fieldOrder = customOptions.field_order;

      this.readFormValues = new ReadFormValues(this.fieldsOptions);
    }

    render(json) {
      if (this.isSuplerForm(json)) { // might be custom-data-only result
        var result = new CreateFormFromJson(this.renderOptionsGetter, this.i18n, this.validatorFnFactories,
          this.fieldsOptions, this.fieldOrder).renderForm(json[FormSections.META], json.main_form);
        this.container.innerHTML = result.html;

        this.initializeValidation(result.formElementDictionary, json);

        var sendController = new SendController(this, result.formElementDictionary, this.sendControllerOptions, this.elementSearch,
          this.validation);
        sendController.attachRefreshListeners();
        sendController.attachActionListeners();
      }

      var customData = this.getCustomData(json);
      if (customData) this.customDataHandlerFn(customData);

      this.afterRenderFn();
    }

    private initializeValidation(formElementDictionary:FormElementDictionary, json) {
      var oldValidation = this.validation;
      this.validation = new Validation(this.elementSearch, formElementDictionary,
        this.validatorRenderOptions, this.i18n, this.readFormValues);

      this.validation.processServer(json.errors);
      if (oldValidation) {
        /* After a form is processed on the server-side, in some cases only validation of filled in fields is done,
         * as the server doesn't know which fields have been "touched". However, this could cause e.g. value-required
         * errors on "touched" fields to disappear, hence we need to re-run the client-side validation on "touched"
         * fields which aren't validated yet validated. */
        this.validation.reprocessClientFrom(oldValidation);
      }
    }

    getValue(selectedActionId:string = null) {
      return this.readFormValues.getValueFrom(this.container, selectedActionId);
    }

    /**
     * @returns True if there were validation errors.
     */
    validate(validationScope:ValidationScope = ValidateAll):boolean {
      return this.validation.processClient(validationScope);
    }

    /**
     * @param json The json received from the server
     * @returns Custom data, if the json contained it, or `null`, if the form does not contain custom data
     */
    getCustomData(json):any {
      if (this.isSuplerForm(json)) {
        return json.custom_data;
      } else {
        return json
      }
    }

    private isSuplerForm(json):boolean {
      return json.is_supler_form === true;
    }
  }
}
