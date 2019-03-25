import React from "react";
import Button from "react-bootstrap/Button";

export class DeleteStepButton extends React.Component {
  deleteStep = () => {
    this.props.fetchForStep(this.props.path, {
      method: "DELETE"
    }).then(this.props.updateTheorem);
  };

  render() {
    return <Button variant="danger" size="sm" className="ml-1"><span className="fas fa-ban" onClick={this.deleteStep}/></Button>
  }
}
