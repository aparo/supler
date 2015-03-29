module Supler {
  export interface ValidationScope {
    shouldValidate(path:string): boolean
  }

  class _ValidateAll implements ValidationScope {
    shouldValidate(path:string):boolean {
      return true;
    }
  }
  export var ValidateAll = new _ValidateAll();

  class _ValidateNone implements ValidationScope {
    shouldValidate(path:string):boolean {
      return false;
    }
  }
  export var ValidateNone = new _ValidateNone();

  export class ValidateInPath implements ValidationScope {
    constructor(private rootPath:string) {
    }

    shouldValidate(path:string):boolean {
      return path && ((path === this.rootPath) ||
        (path.indexOf(this.rootPath + '.') === 0) ||
        (path.indexOf(this.rootPath + '[') === 0));
    }
  }

  export class ValidationScopeParser {
    static fromJson(json:any):ValidationScope {
      switch (json.name) {
        case 'none':
          return ValidateNone;
        case 'all':
          return ValidateAll;
        case 'path':
          return new ValidateInPath(json.path);
        // other validation scopes are unsupported and we fall back to the server validation
        default:
          return ValidateNone;
      }
    }
  }
}
