import React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import Typography from '@material-ui/core/Typography';
import SearchBar from './SearchBar.js'
import CodeGrid from './CodeGrid.js'


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

function ModelSearch() {
  const classes = useStyles();
  return (
    <div className={classes.root}>
    <Typography variant="h3" component="h1" style={{margin: 20}}>
        Model Search
    </Typography>
    <SearchBar></SearchBar>
    <CodeGrid></CodeGrid>
    </div>
  );
}

export default ModelSearch;
