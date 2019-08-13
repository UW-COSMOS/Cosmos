import React, { Component } from 'react';
import { makeStyles } from '@material-ui/core/styles';
import AppBar from '@material-ui/core/AppBar';
import Toolbar from '@material-ui/core/Toolbar';
import Typography from '@material-ui/core/Typography';
import Button from '@material-ui/core/Button';

class NavBar extends Component {
    render() {
        const classes = makeStyles(theme => ({
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

        return (
          <div className={classes.root}>
            <AppBar position="static">
              <Toolbar>
                <Typography variant="h6" className={classes.title}>
                  Cosmos
                </Typography>
                <Button color="inherit" onClick={this.props.qa_fn}>QA</Button>
                <Button color="inherit" onClick={this.props.search_fn}>Search</Button>
                <Button color="inherit" onClick={this.props.visualize_fn}>Visualize</Button>
                <Button color="inherit" onClick={this.props.model_analysis_fn}>Model Analysis</Button>
              </Toolbar>
            </AppBar>
          </div>
        );
    }
}

export default NavBar;
