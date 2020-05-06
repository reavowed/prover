import React from "react";
import Alert from "react-bootstrap/Alert";

export default function ErrorAlert({error, setError}) {
  return error && <Alert variant="danger" onClose={() => setError(null)} dismissible>{error}</Alert>
}
