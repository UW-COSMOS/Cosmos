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


const NUMBER_OF_BINS = 20

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


function handleValues(values){
    values = values.sort()
    var min = values[0]
    var max = values[values.length-1]
    var num_elements = values.length
    var bin_width = (max - min) / NUMBER_OF_BINS
    var bins = []
    for(var i = 0; i < bin_width * NUMBER_OF_BINS; i += bin_width){
        bins.push({
            x0: i,
            x: i + bin_width,
            y: 0
        })
    }

    for(var i = 0; i < values.length; i++){
        var item = values[i]
        for (var j = 0; j < bins.length; j++){
            var bin = bins[j]
            if(item > bin.x0 && item <= bin.x){
                bin.y++;
            }
        }
    }
    return bins
}

export default function ModelAnalysis(props) {
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
  const [answer, setAnswer] = React.useState([])
  const [answerDOI, setAnswerDOI] = React.useState([])
  const [maxY, setMaxY] = React.useState(0)
  const [data, setData] = React.useState([])
  const [hide, setHide] = React.useState(true)

  // histogram cache length
  const MAX_CACHE_LEN = 30

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
    window.scrollTo(0,0)
    var target_q = "What is " + target + "?"
    
    setPhrase(target)
    fetch(`http://localhost:5001/qa?q=${encodeURIComponent(target_q)}`)
    .then(response => response.json())
    .then(data => {
      console.log(data.results)
      if(data.results.length > 0)
      {
        setHide(true)
        setAnswer(data.results)
        for(var i = 0; i < data.results.length; i++){ 
          let pdf_id = data.results[0].pdf_name.slice(0, -4)
          fetch(`https://geodeepdive.org/api/articles?docid=${encodeURIComponent(pdf_id)}`)  
            .then(response => response.json())
            .then(doi_res => {
              let id = doi_res.success.data[0]._gddid
              let title = doi_res.success.data[0].title
              let url = doi_res.success.data[0].link[0].url //`https.doi.org/${doi_res.success.data[0].identifier[0].id}`
              setAnswerDOI(oldValues => [...oldValues, {pdf_id: id, title: title, url: url}])
              console.log(answerDOI)
          })
        }
       
      }
      else
      {
        setHide(false)
      } 
    })
    

    fetch(`http://localhost:5003/word2vec?word=${encodeURIComponent(target)}&n=25`) .then(response => response.json())
    .then(data => {
        console.log('-----')
        console.log(data)
        var d = data.data
        var l = []
        for(var i = 0; i < d.length; i++){
            l.push(d[i])
        }

        setRelatedTerms(l.map(listitem))
    })
    fetch(`http://localhost:5001/search?q=${encodeURIComponent(internal_target)}&type=${encodeURIComponent('Section')}`)
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

    fetch(`http://localhost:5001/search?q=${encodeURIComponent(internal_target)}&type=${encodeURIComponent('EquationContext')}`)
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

    fetch(`http://localhost:5001/search?q=${encodeURIComponent(internal_target)}&type=${encodeURIComponent('FigureContext')}`)
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
    // Check the cache
    var cacheHit = false
    for(var i = 0; i < props.histogramCache.length; i++){
        if(props.histogramCache[i].target === target){
            var objects = props.histogramCache[i].tableObjects
            setTableObjects(objects)
            setMaxY(props.histogramCache[i].y)
            var bins = props.histogramCache[i].bins
            setData(bins)
            for(var i = 0; i < objects.length; i++){
              let pdf_id = objects[i].pdf_name.slice(0, -4)
              fetch(`https://geodeepdive.org/api/articles?docid=${encodeURIComponent(pdf_id)}`)
                .then(response => response.json())
                .then(doi_res => {
                  let id = doi_res.success.data[0]._gddid
                  let title = doi_res.success.data[0].title
                  let url = doi_res.success.data[0].link[0].url //`https.doi.org/${doi_res.success.data[0].identifier[0].id}`
                  setTableDOIs(oldValues => [...oldValues, {pdf_id: id, title: title, url: url}])
                })
            cacheHit = true
        }
    }
    }

    if(cacheHit == false){
        fetch(`http://localhost:5001/values?q=${encodeURIComponent(target)}`)
        .then(response => response.json())
        .then(data => {
          setTableObjects(data.results)
          var bins = handleValues(data.values)
          var y = 0
          for(var i = 0; i < bins.length; i++){
              if(bins[i].y > y)
                  y = bins[i].y
          }
          setMaxY(y)
          setData(bins)
          var obj = {
                    target: target,
                    bins: bins,
                    y: y,
                    tableObjects: data.results
                }
          if(props.histogramCache.length < props.max_cache_len){
              var c = props.histogramCache.slice()
              c.push(obj)
              props.setHistogramCache(c)
          } else{
              var c = props.histogramCache.slice()
              c.pop()
              c.push(obj)
              props.setHistogramCache(c)
          }
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
    }
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
                  maxY={maxY}
                  hide={hide}
                  data={data}
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

