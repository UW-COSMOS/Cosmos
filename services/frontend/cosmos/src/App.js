import React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import QA from './QA.js'
import NavBar from './NavBar.js'

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

function App() {
  const classes = useStyles();
  return (
    <div className={classes.root}>
    <NavBar></NavBar>
    <QA></QA>
    </div>
  );
}

export default App;
