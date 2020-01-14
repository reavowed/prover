import * as React from "react";

export default class SingleActionProver extends React.Component {
  constructor(props) {
    super(props);
    this.state = {};
  }

  componentDidMount() {
    this.setState({loading: true}, () => {
      this.props.prove()
        .catch((error) => this.props.onError(error))
        .then(() => this.setState({loading: false}));
    });
  }

  render() {
    if (this.state.loading) {
      return <div>
        {this.props.loadingText} <span className="fas fa-spin fa-spinner"/>
      </div>
    } else {
      return <></>;
    }
  }
}
