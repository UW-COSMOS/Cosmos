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


const DATA = [
    {x0: 0.0, x: 0.1, y: 4},
    {x0: 0.1, x: 0.2, y: 7},
    {x0: 0.3, x: 0.4, y: 9},
    {x0: 0.4, x: 0.5, y: 7},
    {x0: 0.5, x: 0.6, y: 7},
    {x0: 0.6, x: 0.7, y: 3},
    {x0: 0.7, x: 0.8, y: 2},
    {x0: 0.8, x: 0.9, y: 1},
    {x0: 0.9, x: 1.0, y: 2},
  ];


export default function ModelAnalysis() {
  const classes = useStyles();
  const [activeStep, setActiveStep] = React.useState(0);
  const steps = getSteps();
  const [fileName, setFilename] = React.useState('')
  const [fileContent, setFileContent] = React.useState('')
  const [results, setResults] = React.useState([])
  const [phrase, setPhrase] = React.useState('')
  const [relatedTerms, setRelatedTerms] = React.useState([])
  const [textObjects, setTextObjects] = React.useState([])
  const [textDOIs, setTextDOIs] = React.useState([])
  const [figureObjects, setFigureObjects] = React.useState([])
  const [figureDOIs, setFigureDOIs] = React.useState([])
  const [tableObjects, setTableObjects] = React.useState([])
  const [tableDOIs, setTableDOIs] = React.useState([])
  const [equationObjects, setEquationObjects] = React.useState([])
  const [equationDOIs, setEquationDOIs] = React.useState([])
  const [answer, setAnswer] = React.useState('')
  const [answerDOI, setAnswerDOI] = React.useState('')

  function handleUpload(target){
    var myFile = target.target.files[0];
    var reader = new FileReader();
    reader.readAsText(myFile);
    reader.onload=function(){
      setFilename(myFile.name)
      setFileContent(reader.result)
    }
  }
  function listitem(term){
    return (<ListItem>
              <ListItemText
                primary={"- " + term[0]}
              />
            </ListItem>)
  }

  function handleAnalyzeClick(target){
    var t = target.split(" ")
    var internal_target = target
    if(t.indexOf('TOC') != -1){
      internal_target = 'TOC'
      setAnswer('Total Organic Carbon - The amount of carbon bound in organic compounds in sample. Because all organic compounds include carbon as the common element, total organic carbon measurements provide a fundamental means of assessing the degree of organic pollution.')
      setAnswerDOI('http://www.sciencedirect.com/science/article/pii/B9780750675079500103')
    } else if(t.length > 1){
      internal_target = t[0]
    }
    window.scrollTo(0,0)
    
    setPhrase(target)
    console.log(target)
    let proxyUrl = 'https://cors-anywhere.herokuapp.com/'
    let targetUrl = `http://teststrata.geology.wisc.edu/xdd_v1/word2vec?word=${encodeURIComponent(internal_target)}&n=10`
    console.log(proxyUrl + targetUrl)
    fetch(proxyUrl + targetUrl)
        .then(res => res.json())
        .then(res => {
            console.log(res)
            setRelatedTerms(res.data.map(listitem))
        })

    fetch(`http://localhost:5001/search?q=${encodeURIComponent(internal_target)}&type=${encodeURIComponent('Body text')}`)
    .then(response => response.json())
    .then(data => {
      setTextObjects(data.results)
      for(var i = 0; i < data.results.length; i++){
        let pdf_id = data.results[i].pdf_name.slice(0, -4)
        fetch(`https://geodeepdive.org/api/articles?docid=${encodeURIComponent(pdf_id)}`)
          .then(response => response.json())
          .then(doi_res => {
            let id = doi_res.success.data[0]._gddid
            let title = doi_res.success.data[0].title
            let url = doi_res.success.data[0].link[0].url //`https.doi.org/${doi_res.success.data[0].identifier[0].id}`
            setTextDOIs(oldValues => [...oldValues, {pdf_id: id, title: title, url: url}])
          })
      }
    })

    fetch(`http://localhost:5001/search?q=${encodeURIComponent(internal_target)}&type=${encodeURIComponent('equation')}`)
    .then(response => response.json())
    .then(data => {
      setEquationObjects(data.results)
      for(var i = 0; i < data.results.length; i++){
        let pdf_id = data.results[i].pdf_name.slice(0, -4)
        fetch(`https://geodeepdive.org/api/articles?docid=${encodeURIComponent(pdf_id)}`)
          .then(response => response.json())
          .then(doi_res => {
            let id = doi_res.success.data[0]._gddid
            let title = doi_res.success.data[0].title
            let url = doi_res.success.data[0].link[0].url //`https.doi.org/${doi_res.success.data[0].identifier[0].id}`
            setEquationDOIs(oldValues => [...oldValues, {pdf_id: id, title: title, url: url}])
          })
      }
    })

    fetch(`http://localhost:5001/search?q=${encodeURIComponent(internal_target)}&type=${encodeURIComponent('figure')}`)
    .then(response => response.json())
    .then(data => {
      setFigureObjects(data.results)
      for(var i = 0; i < data.results.length; i++){
        let pdf_id = data.results[i].pdf_name.slice(0, -4)
        fetch(`https://geodeepdive.org/api/articles?docid=${encodeURIComponent(pdf_id)}`)
          .then(response => response.json())
          .then(doi_res => {
            let id = doi_res.success.data[0]._gddid
            let title = doi_res.success.data[0].title
            let url = doi_res.success.data[0].link[0].url //`https.doi.org/${doi_res.success.data[0].identifier[0].id}`
            setFigureDOIs(oldValues => [...oldValues, {pdf_id: id, title: title, url: url}])
          })
      }
    })

    fetch(`http://localhost:5001/search?q=${encodeURIComponent(internal_target)}&type=${encodeURIComponent('table')}`)
    .then(response => response.json())
    .then(data => {
      setTableObjects(data.results)
      for(var i = 0; i < data.results.length; i++){
        let pdf_id = data.results[i].pdf_name.slice(0, -4)
        fetch(`https://geodeepdive.org/api/articles?docid=${encodeURIComponent(pdf_id)}`)
          .then(response => response.json())
          .then(doi_res => {
            let id = doi_res.success.data[0]._gddid
            let title = doi_res.success.data[0].title
            let url = doi_res.success.data[0].link[0].url //`https.doi.org/${doi_res.success.data[0].identifier[0].id}`
            setTableDOIs(oldValues => [...oldValues, {pdf_id: id, title: title, url: url}])
          })
      }
    })
    handleNext()
  }

  function getStepContent(stepIndex) {
    switch (stepIndex) {
      case 0:
        return (<Upload handleUpload={handleUpload} filename={fileName} handleNext={handleNext}></Upload>)
      case 1:
        return (<CodeGrid data={results} handleClick={handleAnalyzeClick}></CodeGrid>)//(<div><SyntaxHighlighter language='fortran' styles={{margin: 20}}>{fileContent}</SyntaxHighlighter></div>);
      case 2:
        return <PhraseAnalysis 
                  answer={answer}
                  answerDOI={answerDOI}
                  phrase={phrase} 
                  relatedTerms={relatedTerms}
                  tableObjects={tableObjects}
                  tableDOIs={tableDOIs}
                  figureObjects={figureObjects}
                  figureDOIs={figureDOIs}
                  equationObjects={equationObjects}
                  equationDOIs={equationDOIs}
                  textObjects={textObjects}
                  textDOIs={textDOIs}></PhraseAnalysis>;
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

