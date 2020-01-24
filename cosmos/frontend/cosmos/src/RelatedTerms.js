import React from 'react';
import Grid from '@material-ui/core/Grid';
import Typography from '@material-ui/core/Typography';
import { makeStyles } from '@material-ui/core/styles';
import CircularProgress from '@material-ui/core/CircularProgress';
import Hidden from '@material-ui/core/Hidden';


const useStyles = makeStyles(theme => ({
  demo: {
    backgroundColor: theme.palette.background.paper,
  },
  container: {
      flexbox: 1,
      margin: 20
  }
}));

export default function RelatedTerms(props){
    const classes = useStyles();
    return (<div className={classes.containter}>
          <Typography variant="h4" component="h4">
            Related Terms
          </Typography>
          <Hidden xlDown={props.relatedTerms.length !== 0 || props.hideProgress}>
            <CircularProgress color="secondary" />
          </Hidden>
          <div className={classes.demo}>
          {props.relatedTerms}
          </div></div>
        )
}

