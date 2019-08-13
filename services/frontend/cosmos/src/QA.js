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


const useStyles = makeStyles(theme => ({
  root: {
    flexGrow: 1,
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

    function onEnter(query){
        let proxyUrl = 'https://cors-anywhere.herokuapp.com/'
        let targetUrl = `http://teststrata.geology.wisc.edu/xdd_v1/word2vec?word=${encodeURIComponent(query)}&n=10`
        let res = fetch(proxyUrl + targetUrl)
            .then(res => res.json())
            .then(res => {
                setRelatedTerms(res.data.map(listitem))
            }
            )
    }
    return (
    <div className={classes.root}>
    <Typography variant="h3" component="h1" style={{margin: 20}}>
        Question Answering and Query Refinement
    </Typography>
    <SearchBar enter_fn={onEnter}></SearchBar>
    <Typography variant="h4" component="h4" style={{margin: 20}}>
        Answer
    </Typography>
    <QAAnswer answer='PLACEHOLDER ANSWER' doi='https://example.com'></QAAnswer>
    <Grid item xs={12} md={6} style={{margin: 20}}>
          <Typography variant="h4" component="h4">
            Related Terms
          </Typography>
          <div className={classes.demo}>
          {relatedTerms}
          </div>
        </Grid>
    </div>
  );
}

export default QA;
