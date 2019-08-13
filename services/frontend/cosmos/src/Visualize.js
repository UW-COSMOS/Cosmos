import React, { useState } from 'react';
import { makeStyles } from '@material-ui/core/styles';
import Typography from '@material-ui/core/Typography';
import SearchBar from './SearchBar.js'
import Histogram from './Histogram.js'
import ObjectGrid from './ObjectGrid.js'
import Hidden from '@material-ui/core/Hidden';


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

const DATA = [
    {x0: 0, x: 1, y: 2},
    {x0: 1, x: 2, y: 2},
    {x0: 2, x: 3, y: 3},
    {x0: 3, x: 4, y: 1},
    {x0: 4, x: 5, y: 0},
    {x0: 5, x: 6, y: 2},
    {x0: 6, x: 7, y: 1},
    {x0: 7, x: 8, y: 1},
  ];
function Visualize() {
  const classes = useStyles();
  const [hide, setHide] = useState(true)
  const [results, setResults] = React.useState([])
  const [doiResults, setDoi] = React.useState([])
  const [values, setValues] = React.useState({
    type: 'Body text',
    query: '',
  });
  function onEnter(query){
      setValues({...values, ['query']: query})
      setResults([])
      setDoi([])
      fetch(`http://localhost:5001/search?q=${encodeURIComponent(query)}&type=${encodeURIComponent('table')}`)
      .then(response => response.json())
      .then(data => {
        setResults(data.results)
        for(var i = 0; i < data.results.length; i++){
          let pdf_id = data.results[i].pdf_name.slice(0, -4)
          fetch(`https://geodeepdive.org/api/articles?docid=${encodeURIComponent(pdf_id)}`)
            .then(response => response.json())
            .then(doi_res => {
              let id = doi_res.success.data[0]._gddid
              let title = doi_res.success.data[0].title
              let url = doi_res.success.data[0].link[0].url //`https.doi.org/${doi_res.success.data[0].identifier[0].id}`
              setDoi(oldValues => [...oldValues, {pdf_id: id, title: title, url: url}])
              setHide(false)
            })
        }
      })
  }
  return (
    <div className={classes.root}>
    <Typography variant="h3" component="h1" style={{margin: 20}}>
        Knowledge Visualization
    </Typography>
    <SearchBar enter_fn={onEnter}></SearchBar>
    <Hidden xlDown={hide}>
    <Typography variant="h4" component="h4" className={classes.text}>
      Extracted empirical distribution over query
    </Typography>
    <Histogram data={DATA} xdomain={[0,8]} ydomain={[0, 4]}></Histogram>
    <Typography variant="h4" component="h4" className={classes.text}>
      Extracted items
    </Typography>
    <ObjectGrid objects={results} dois={doiResults}></ObjectGrid>
    </Hidden>
    </div>
  );
}

export default Visualize;
