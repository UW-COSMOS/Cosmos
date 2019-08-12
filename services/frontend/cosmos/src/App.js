import React, { Component } from 'react';
import { makeStyles } from '@material-ui/core/styles';
import QA from './QA.js'
import Search from './Search.js'
import NavBar from './NavBar.js'
import Visualize from './Visualize.js'
import ModelSearch from './ModelSearch.js'


const view_states = {
  QA: 'qa',
  SEARCH: 'search',
  VISUALIZE: 'visualize',
  MODEL_SEARCH: 'model_search'
}


class App extends Component{
  constructor(props) {
    super(props)
    this.set_qa_state = this.set_qa_state.bind(this)
    this.set_search_state = this.set_search_state.bind(this)
    this.set_visualize_state = this.set_visualize_state.bind(this)
    this.set_ms_state = this.set_ms_state.bind(this)
    this.state = {view: view_states.QA}
  }

  set_qa_state(){
    this.setState({
      view: view_states.QA
    })
  }

  set_search_state(){
    this.setState({
      view: view_states.SEARCH
    })
  }

  set_visualize_state(){
    this.setState({
      view: view_states.VISUALIZE
    })
  }

  set_ms_state(){
    this.setState({
      view: view_states.MODEL_SEARCH
    })
  }

  show_state(){
    switch(this.state.view){
      case view_states.QA:
        return (<QA></QA>)
      case view_states.SEARCH:
        return (<Search></Search>)
      case view_states.VISUALIZE:
        return (<Visualize></Visualize>)
      case view_states.MODEL_SEARCH:
        return (<ModelSearch></ModelSearch>)
      default:
        return (<p>Invalid view state</p>)
    }
  }

  render() {
    const classes = makeStyles(theme => ({
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
    return (
      <div className={classes.root}>
      <NavBar qa_fn={this.set_qa_state} search_fn={this.set_search_state} visualize_fn={this.set_visualize_state} model_search_fn={this.set_ms_state}></NavBar>
      {this.show_state()}
      </div>
    );
  }
}

export default App;
