import React, { Component } from 'react';
import { makeStyles } from '@material-ui/core/styles';
import QA from './QA.js'
import Search from './Search.js'
import NavBar from './NavBar.js'
import Visualize from './Visualize.js'
import ModelAnalysis from './ModelAnalysis.js'


const view_states = {
  QA: 'qa',
  SEARCH: 'search',
  VISUALIZE: 'visualize',
  MODEL_ANALYSIS: 'model_analysis'
}


class App extends Component{
  constructor(props) {
    super(props)
    this.set_qa_state = this.set_qa_state.bind(this)
    this.set_search_state = this.set_search_state.bind(this)
    this.set_visualize_state = this.set_visualize_state.bind(this)
    this.set_ms_state = this.set_ms_state.bind(this)
    this.set_histogram_cache = this.set_histogram_cache.bind(this)
    this.state = {view: view_states.QA, histogramCache: []}
  }

  set_histogram_cache(cache){
      this.setState((state, props) => {
          return {view: state.view, histogramCache: cache}
      })
  }

  set_qa_state(){
    this.setState((state, props) => {
      return {
          view: view_states.QA,
          histogramCache: state.histogramCache
      }
    })
  }

  set_search_state(){
    this.setState((state, props) => {
      return {
          view: view_states.SEARCH,
          histogramCache: state.histogramCache
      }
    })
  }

  set_visualize_state(){
    this.setState((state, props) => {
      return {
          view: view_states.VISUALIZE,
          histogramCache: state.histogramCache
      }
    })
  }

  set_ms_state(){
    this.setState((state, props) => {
      return {
         view: view_states.MODEL_ANALYSIS,
         histogramCache: state.histogramCache
      }
    })
  }

  show_state(){
    switch(this.state.view){
      case view_states.QA:
        return (<QA></QA>)
      case view_states.SEARCH:
        return (<Search></Search>)
      case view_states.VISUALIZE:
        return (<Visualize histogramCache={this.state.histogramCache} setHistogramCache={this.set_histogram_cache} max_cache_len={30}></Visualize>)
      case view_states.MODEL_ANALYSIS:
        return (<ModelAnalysis histogramCache={this.state.histogramCache} setHistogramCache={this.set_histogram_cache} max_cache_len={30}></ModelAnalysis>)
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
      <NavBar qa_fn={this.set_qa_state} search_fn={this.set_search_state} visualize_fn={this.set_visualize_state} model_analysis_fn={this.set_ms_state}></NavBar>
      {this.show_state()}
      </div>
    );
  }
}

export default App;
