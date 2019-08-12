import React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import Typography from '@material-ui/core/Typography';
import Paper from '@material-ui/core/Paper';
import Grid from '@material-ui/core/Grid';
import List from '@material-ui/core/List';
import ListItem from '@material-ui/core/ListItem';
import ListItemText from '@material-ui/core/ListItemText';
import SearchBar from './SearchBar.js'


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

function populate_related_term(term){
  return (<ListItem>
            <ListItemText
              primary={"- " + term}
            />
          </ListItem>)
}

function generate_list(){
  let related_terms = ['term1', 'term2', 'term3']
  let mapped = related_terms.map(populate_related_term)
  return (<List>{mapped}</List>)
}

function onEnter(query){
    alert(query);
}

function QA() {
  const classes = useStyles();
  return (
    <div className={classes.root}>
    <Typography variant="h3" component="h1" style={{margin: 20}}>
        Question Answering and Query Refinement
    </Typography>
    <SearchBar enter_fn={onEnter}></SearchBar>
    <Typography variant="h4" component="h4" style={{margin: 20}}>
        Answer
    </Typography>
    <Paper className={classes.container}>
      <Typography variant="p" component="h3">
        Placeholder answer to the question.
      </Typography>
      <Typography component="p">
        DOI: myref.com
      </Typography>
    </Paper>
    <Grid item xs={12} md={6} style={{margin: 20}}>
          <Typography variant="h4" component="h4">
            Related Terms
          </Typography>
          <div className={classes.demo}>
          {generate_list()}
          </div>
        </Grid>
    </div>
  );
}

export default QA;
