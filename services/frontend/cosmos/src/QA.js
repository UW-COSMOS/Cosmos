import React, { useState } from 'react';
import { makeStyles } from '@material-ui/core/styles';
import Typography from '@material-ui/core/Typography';
import Paper from '@material-ui/core/Paper';
import Grid from '@material-ui/core/Grid';
import List from '@material-ui/core/List';
import ListItem from '@material-ui/core/ListItem';
import ListItemText from '@material-ui/core/ListItemText';
import SearchBar from './SearchBar.js'
import QAAnswer from './QAAnswer.js'
import RelatedTerms from './RelatedTerms.js'
import Hidden from '@material-ui/core/Hidden';
import AnswerGrid from './AnswerGrid.js'

const useStyles = makeStyles(theme => ({
  root: {
    flexGrow: 1,
    margin: 20
  },
  container: {
    padding: '2px 4px',
    margin: 20
  },
  demo: {
    backgroundColor: theme.palette.background.paper,
  },
}));

function listitem(term){
  return (<ListItem>
            <ListItemText
              primary={"- " + term[0]}
            />
          </ListItem>)
}

function QA() {
    const classes = useStyles();
    const [values, setValues] = React.useState({
                query: '',
          });
    const [relatedTerms, setRelatedTerms] = useState([])
    const [answer, setAnswer] = useState([])
    const [answerDOI, setAnswerDOI] = useState([])
    function handleChange(event) {
          setValues(oldValues => ({
                    ...oldValues,
                    [event.target.name]: event.target.value,
                  }));
        }
    function onEnter(query){
        setValues({...values, ['query']: query})
        setAnswer([])
        setAnswerDOI([])
        fetch(`http://localhost:5001/qa?q=${encodeURIComponent(query)}`)
        .then(response => response.json())
        .then(data => {
          console.log(data.results)
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
        })
    }
    return (
    <div className={classes.root}>
    <Typography variant="h3" component="h1" style={{margin: 20}}>
        Question Answering and Query Refinement
    </Typography>
    <SearchBar enter_fn={onEnter}></SearchBar>
    <div className={classes.container}>
    <AnswerGrid objects={answer} dois={answerDOI}></AnswerGrid>
    <RelatedTerms relatedTerms={relatedTerms} hideProgress={true}></RelatedTerms>
    </div>
    </div>
  );
}

export default QA;
