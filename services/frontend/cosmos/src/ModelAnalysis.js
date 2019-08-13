import React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import Stepper from '@material-ui/core/Stepper';
import Step from '@material-ui/core/Step';
import StepLabel from '@material-ui/core/StepLabel';
import Button from '@material-ui/core/Button';
import Typography from '@material-ui/core/Typography';
import CodeGrid from './CodeGrid.js'
import PhraseAnalysis from './PhraseAnalysis.js'

const useStyles = makeStyles(theme => ({
  root: {
    width: '90%',
  },
  container: {
    display: "flex",
    alignItems: "center",
    justifyContent: "center",

  },
  backButton: {
    marginRight: theme.spacing(1),
  },
  instructions: {
    marginTop: theme.spacing(1),
    marginBottom: theme.spacing(1),
  },
  input: {
    display: 'none',
  },
  button: {
    margin: 20,
  },
  instructions: {
    margin: 20,
  },

}));

function getSteps() {
  return ['Code input', 'Entity Selection', 'Analysis'];
}


function Upload(props){
  const classes = useStyles();
  return (<div><input
            accept=".f90"
            className={classes.input}
            id="outlined-button-file"
            multiple
            type="file"
            onChange={props.handleUpload}
          />
          <label htmlFor="outlined-button-file">
            <Button variant="contained" color="primary" component="span" className={classes.button}>
              Upload
            </Button>
          </label>
          <Typography variant="h5" component="p" className={classes.button}>{props.filename}</Typography>
          <Button variant="contained" color="primary" onClick={props.handleNext} className={classes.button}>
            Analyze
          </Button></div>
        );
}


export default function ModelAnalysis() {
  const classes = useStyles();
  const [activeStep, setActiveStep] = React.useState(0);
  const steps = getSteps();
  const [fileName, setFilename] = React.useState('')
  const [fileContent, setFileContent] = React.useState('')
  const [results, setResults] = React.useState([])
  const [phrase, setPhrase] = React.useState('')

  function handleUpload(target){
    var myFile = target.target.files[0];
    var reader = new FileReader();
    reader.readAsText(myFile);
    reader.onload=function(){
      setFilename(myFile.name)
      setFileContent(reader.result)
    }
  }

  function handleAnalyzeClick(phrase){
    setPhrase(phrase)
    handleNext()
  }

  function getStepContent(stepIndex) {
    switch (stepIndex) {
      case 0:
        return (<Upload handleUpload={handleUpload} filename={fileName} handleNext={handleNext}></Upload>)
      case 1:
        return (<CodeGrid data={results} handleClick={handleAnalyzeClick}></CodeGrid>)//(<div><SyntaxHighlighter language='fortran' styles={{margin: 20}}>{fileContent}</SyntaxHighlighter></div>);
      case 2:
        return <PhraseAnalysis phrase={phrase}></PhraseAnalysis>;
      default:
        return 'Uknown stepIndex';
    }
  }
  function handleNext() {
    setActiveStep(prevActiveStep => {
      if (prevActiveStep == 0){
        fetch('http://localhost:5001/analyze', {
          method: 'post',
          headers: {
            'Accept': 'application/json, text/plain, */*',
            'Content-Type': 'application/json',
            'Access-Control-Allow-Origin': '*',
            'Access-Control-Request-Method': 'POST'
          },
          body: JSON.stringify({file: fileContent})
        }).then(resp => resp.json())
          .then(resp => {
            setResults(resp.results)
            console.log(resp.results)
          })
      }
      return prevActiveStep + 1
    });
  }

  function handleBack() {
    setActiveStep(prevActiveStep => prevActiveStep - 1);
  }

  function handleReset() {
    setActiveStep(0);
  }

  return (
    <div className={classes.root}>
      <Stepper activeStep={activeStep} alternativeLabel>
        {steps.map(label => (
          <Step key={label}>
            <StepLabel>{label}</StepLabel>
          </Step>
        ))}
      </Stepper>
      <div>
        {activeStep === steps.length ? (
          <div>
            <Typography className={classes.instructions}>All steps completed</Typography>
            <Button onClick={handleReset}>Reset</Button>
          </div>
        ) : (
          <div className={classes.container}>
            {getStepContent(activeStep)}
          </div>
        )}
      </div>
    </div>
  );
}

