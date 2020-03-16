import React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import Stepper from '@material-ui/core/Stepper';
import Step from '@material-ui/core/Step';
import StepLabel from '@material-ui/core/StepLabel';
import Button from '@material-ui/core/Button';
import Typography from '@material-ui/core/Typography';
import CodeGrid from './CodeGrid.js'
import PhraseAnalysis from './PhraseAnalysis.js'
import ListItem from '@material-ui/core/ListItem';
import ListItemText from '@material-ui/core/ListItemText';

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
  return ['Choose document', 'Ingest', 'Process', 'Complete!'];
}


function Upload(props){
  const classes = useStyles();
  return (<div><input
            accept=".pdf"
            className={classes.input}
            id="outlined-button-file"
//            multiple
            type="file"
            onChange={props.handleUpload}
          />
          <label htmlFor="outlined-button-file">
            <Button variant="contained" color="primary" component="span" className={classes.button}>
              Choose PDF
            </Button>
          </label>
          <Typography variant="h5" component="p" className={classes.button}>{props.filename}</Typography>
          <Button variant="contained" color="primary" onClick={props.handleNext} className={classes.button}>
            Process
          </Button></div>
        );
}

export default function UploadPdf(props) {
  const classes = useStyles();
  const [activeStep, setActiveStep] = React.useState(0);
  const steps = getSteps();
  const [fileName, setFilename] = React.useState('')
  const [fileContent, setFileContent] = React.useState('')
  const [results, setResults] = React.useState([])

  function handleUpload(target){
      console.log(target)
    var myFile = target.target.files[0];
    var reader = new FileReader();
//        bstring = base64.b64encode(rf.read()).decode()
    reader.readAsDataURL(myFile);
    reader.onload=function(){
      setFilename(myFile.name)
        setFileContent(reader.result.toString().replace(/^data:(.*,)?/, '')) //strip out the data headers but keep the b64 contents
    }
    console.log("File loaded.")
    console.log(fileName)
      console.log(typeof(fileContent))
  }
//  function listitem(term){
//    return (<ListItem>
//              <ListItemText
//                primary={"- " + term[0]}
//              />
//            </ListItem>)
//  }

  function handleAnalyzeClick(target){
      console.log(target)
      console.log(fileName)
  }

  function getStepContent(stepIndex) {
      console.log(`stepIndex: ${stepIndex}`)
    switch (stepIndex) {
      case 0:
        return (<Upload handleUpload={handleUpload} filename={fileName} handleNext={handleNext}></Upload>)
      case 1:
        console.log("I should send things to the server now?")
        return (<CodeGrid data={results} handleClick={handleAnalyzeClick}></CodeGrid>)//(<div><SyntaxHighlighter language='fortran' styles={{margin: 20}}>{fileContent}</SyntaxHighlighter></div>);
//      case 2:
//        return <PhraseAnalysis 
//                  maxY={maxY}
//                  hide={hide}
//                  data={data}
//                  answer={answer}
//                  answerDOI={answerDOI}
//                  phrase={phrase} 
//                  relatedTerms={relatedTerms}
//                  tableObjects={tableObjects}
//                  tableDOIs={tableDOIs}
//                  figureObjects={figureObjects}
//                  figureDOIs={figureDOIs}
//                  equationObjects={equationObjects}
//                  equationDOIs={equationDOIs}
//                  textObjects={textObjects}
//                  textDOIs={textDOIs}></PhraseAnalysis>;
      default:
        return 'Unknown stepIndex';
    }
  }
  function handleNext() {
    console.log("handleNext")
    setActiveStep(prevActiveStep => {
      if (prevActiveStep == 0){
          console.log(fileName)
          console.log(fileContent)
//        result = requests.post('http://ingestion:8000/preprocess', json={'pdf': bstring, 'dataset_id': dataset_id, 'pdf_name': os.path.basename(filename)})
          // TODO: don't love doing this all on localhost with port forwarding, but CORS stuff wasn't working..
          fetch('http://localhost/ingestion/preprocess', {
          method: 'post',
          headers: {
            'Accept': 'application/json, text/plain, */*',
            'Content-Type': 'application/json',
            'Access-Control-Allow-Origin': '*',
            'Access-Control-Request-Method': 'POST'
          },
            body: JSON.stringify({
                'pdf' : fileContent,
                'dataset_id' : 'dummy',
                'pdf_name' : fileName ,
            })
        }).then(resp => resp.json())
          .then(resp => {
//            setResults(resp.results)
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

