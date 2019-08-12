import React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import Typography from '@material-ui/core/Typography';
import SearchBar from './SearchBar.js'
import ObjectGrid from './ObjectGrid.js'


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


function Search() {
  const classes = useStyles();
  return (
    <div className={classes.root}>
    <Typography variant="h3" component="h1" style={{margin: 20}}>
        Knowledge Search
    </Typography>
    <SearchBar></SearchBar>
    <div className={classes.container}>
    <ObjectGrid></ObjectGrid>
    </div>
    </div>
  );
}

export default Search;