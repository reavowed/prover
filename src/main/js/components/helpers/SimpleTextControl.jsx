import React from "react";
import Form from "react-bootstrap/Form";

export default React.forwardRef(function SimpleTextControl({onChange, ...otherProps}, ref) {
  const props = {type: "text", ...otherProps, onChange: (e) => onChange(e.target.value)};
  return <Form.Control ref={ref} {...props} />
});
