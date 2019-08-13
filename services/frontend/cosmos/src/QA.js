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
    const [relatedTerms, setRelatedTerms] = useState([])
    const [hide, setHide] = useState(true)
    const [answer, setAnswer] = useState('')
    const [answerDOI, setAnswerDOI] = useState('')

    function onEnter(query){
        if (query == 'What is TOC?'){
            setAnswer('Total Organic Carbon - The amount of carbon bound in organic compounds in sample. Because all organic compounds include carbon as the common element, total organic carbon measurements provide a fundamental means of assessing the degree of organic pollution.')
            setAnswerDOI('http://www.sciencedirect.com/science/article/pii/B9780750675079500103')
            query = 'TOC'
        } else if (query == 'What is THAA?'){
            setAnswer('total hydrolyzable amino acids (THAA)')
            setAnswerDOI('http://doi.wiley.com/10.1111/j.1745-6584.2008.00493.x')
            query = 'THAA'
        }
        let proxyUrl = 'https://cors-anywhere.herokuapp.com/'
        let targetUrl = `http://teststrata.geology.wisc.edu/xdd_v1/word2vec?word=${encodeURIComponent(query)}&n=10`
        console.log(proxyUrl + targetUrl)
        fetch(proxyUrl + targetUrl)
            .then(res => res.json())
            .then(res => {
                setRelatedTerms(res.data.map(listitem))
                setHide(false)
            })
    }
    return (
    <div className={classes.root}>
    <Typography variant="h3" component="h1" style={{margin: 20}}>
        Question Answering and Query Refinement
    </Typography>
    <SearchBar enter_fn={onEnter}></SearchBar>
    <Hidden xlDown={hide}>
    <Typography variant="h4" component="h4">
        Answer
    </Typography>
    <QAAnswer answer={answer} doi={answerDOI}></QAAnswer>
    <RelatedTerms relatedTerms={relatedTerms} hideProgress={true}></RelatedTerms>
    </Hidden>
    </div>
  );
}

export default QA;
