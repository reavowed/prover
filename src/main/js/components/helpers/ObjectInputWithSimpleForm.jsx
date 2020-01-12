import React, {createRef} from "react";
import Button from "react-bootstrap/Button";
import SimpleControlGroup from "./SimpleControlGroup";

export default class ObjectInputWithSimpleForm extends React.Component {
  constructor(props) {
    super(props);
    this.ref = createRef();
    this.firstInputRef = createRef();
    this.state = {
      object: _.fromPairs(_.map(props.values, v => [v.key, v.initialValue || ""])),
      saving: false
    };
  }

  componentDidMount() {
    this.ref.current.scrollIntoView();
    if (this.firstInputRef.current) {
      this.firstInputRef.current.focus();
    }
  }

  save = () => {
    this.setState({saving: true}, () => {
      this.props.save({...this.state.object})
        .then(this.props.onCancel)
        .catch(() => {})
        .then(() => this.setState({saving: false}));
    });
  };

  render() {
    const {description, values, onCancel} = this.props;
    const {saving, object} = this.state;

    return <div ref={this.ref}>
      <h4>Add {description}</h4>
      {_.map(values, (value, index) => {
        const {key, title, inputType, inputProps} = value;
        return <SimpleControlGroup key={key} title={title} inputType={inputType} {...inputProps} value={object[key]} onChange={value => this.setState({object: {...object, [key]: value}})} ref={index === 0 ? this.firstInputRef : null} readonly={saving}/>
      })}
      <Button size="sm" onClick={this.save} disabled={saving}>Save {description}</Button>
      <Button size="sm" className="ml-2" variant="danger" onClick={onCancel} disabled={saving}>Cancel</Button>
    </div>
  }
}
