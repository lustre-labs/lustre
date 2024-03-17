import { register } from '../dev/javascript/lustre/client-component.ffi.mjs';
import { name, {component_name} as component } from '../dev/javascript/{app_name}/{module_path}.mjs';

register(component(), name);