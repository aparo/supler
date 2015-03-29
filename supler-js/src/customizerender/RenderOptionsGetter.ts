module Supler {
  export class RenderOptionsGetter {
    constructor(private fallbackRenderOptions:RenderOptions, private modifiers:RenderModifierWithMatcher[]) {
    }

    /**
     * Applies all matching render templates for the given field to the default render options.
     * Hence if a template is defined later, it has priority.
     */
    forField(path:string, type:string, renderHintName:string):RenderOptions {
      var current = this.fallbackRenderOptions;
      for (var i = 0; i < this.modifiers.length; i++) {
        var modifier = this.modifiers[i];
        if (modifier.matcher.matches(path, type, renderHintName)) {
          current = modifier.renderOptionsModifier(current);
        }
      }

      return current;
    }

    defaultRenderOptions(): RenderOptions {
      return this.fallbackRenderOptions;
    }

    static parse(defaultRenderOptions:RenderOptions, container:HTMLElement, fieldsOptions:FieldsOptions,
      fieldTemplatesOption: string[]) {

      var allModifiers = [];

      // currently any modifiers in field options have precedence. But maybe we should sort somehow, so that e.g.
      // templates for specific fields should have precedence over templates for render hints?
      allModifiers = allModifiers.concat(new RenderModifiersFromFieldOptions(fieldsOptions).parse());
      allModifiers = allModifiers.concat(new HTMLRenderTemplateParser(container).parse());

      (fieldTemplatesOption || []).forEach(templateId => {
        var element = document.getElementById(templateId);
        if (element) {
          allModifiers = allModifiers.concat(new HTMLRenderTemplateParser(element).parse());
        }
      });

      return new RenderOptionsGetter(defaultRenderOptions, allModifiers);
    }
  }
}
