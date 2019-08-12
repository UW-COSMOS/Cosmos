import React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import Typography from '@material-ui/core/Typography';
import SearchBar from './SearchBar.js'
import Histogram from './Histogram.js'


const useStyles = makeStyles(theme => ({
  root: {
    flexGrow: 1,
  },
  container: {
    padding: '2px 4px',
    margin: 20
  },
  text: {
      margin: 20
  },
  demo: {
    backgroundColor: theme.palette.background.paper,
  },
}));

function Visualize() {
  const classes = useStyles();
  return (
    <div className={classes.root}>
    <Typography variant="h3" component="h1" style={{margin: 20}}>
        Knowledge Visualization
    </Typography>
    <SearchBar></SearchBar>
    <Typography variant="h4" component="h4" className={classes.text}>
      Extracted empirical distribution over query
    </Typography>
    <Histogram></Histogram>
    <Typography variant="h4" component="h4" className={classes.text}>
      Extracted items
    </Typography>
    </div>
  );
}

export default Visualize;
