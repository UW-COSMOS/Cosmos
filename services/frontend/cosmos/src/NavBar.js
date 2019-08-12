import React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import AppBar from '@material-ui/core/AppBar';
import Toolbar from '@material-ui/core/Toolbar';
import Typography from '@material-ui/core/Typography';
import Button from '@material-ui/core/Button';

const useStyles = makeStyles(theme => ({
  root: {
    flexGrow: 1,
  },
  menuButton: {
    marginRight: theme.spacing(2),
  },
  title: {
    flexGrow: 1,
  },
}));

export default function NavBar(qa_fn, search_fn, visualize_fn, model_search_fn) {
  const classes = useStyles();

  return (
    <div className={classes.root}>
      <AppBar position="static">
        <Toolbar>
          <Typography variant="h6" className={classes.title}>
            Cosmos
          </Typography>
          <Button color="inherit" onClick={qa_fn}>QA</Button>
          <Button color="inherit" onClick={search_fn}>Search</Button>
          <Button color="inherit" onClick={visualize_fn}>Visualize</Button>
          <Button color="inherit" onClick={model_search_fn}>Model Search</Button>
        </Toolbar>
      </AppBar>
    </div>
  );
}