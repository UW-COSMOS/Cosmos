import React, { useState } from 'react';
import { makeStyles } from '@material-ui/core/styles';
import Typography from '@material-ui/core/Typography';
import SearchBar from './SearchBar.js'
import Histogram from './Histogram.js'
import ObjectGrid from './ObjectGrid.js'
import Hidden from '@material-ui/core/Hidden';
import CircularProgress from '@material-ui/core/CircularProgress';


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

const NUMBER_OF_BINS = 20

function handleValues(values){
    values = values.sort()
    var min = values[0]
    var max = values[values.length-1]
    var num_elements = values.length
    var bin_width = (max - min) / NUMBER_OF_BINS
    var bins = []
    for(var i = 0; i < bin_width * NUMBER_OF_BINS; i += bin_width){
        bins.push({
            x0: i,
            x: i + bin_width,
            y: 0
        })
    }

    for(var i = 0; i < values.length; i++){
        var item = values[i]
        for (var j = 0; j < bins.length; j++){
            var bin = bins[j]
            if(item > bin.x0 && item <= bin.x){
                bin.y++;
            }
        }
    }
    return bins
}
function Visualize() {
  const classes = useStyles();
  const [hide, setHide] = useState(true)
  const [results, setResults] = React.useState([])
  const [doiResults, setDoi] = React.useState([])
  const [maxY, setMaxY] = React.useState(0)
  const [data, setData] = React.useState([])
  const [processing, setProcessing] = React.useState(true)
  const [noResults, setNoResults] = React.useState(true)
  const [values, setValues] = React.useState({
    query: '',
  });
  function onEnter(query){
      setValues({...values, ['query']: query})
      setResults([])
      setDoi([])
      setProcessing(false)
      setNoResults(true)
      fetch(`http://localhost:5001/values?q=${encodeURIComponent(query)}`)
      .then(response => response.json())
      .then(data => {
        setResults(data.results)
        var bins = handleValues(data.values)
        var y = 0
        for(var i = 0; i < bins.length; i++){
            if(bins[i].y > y)
                y = bins[i].y
        }
        setMaxY(y)
        setData(bins)
        console.log(bins)
        if(bins.length == 0){
            setProcessing(true)
            setNoResults(false)
        }
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
              setProcessing(true)
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
    <Hidden xlDown={processing}>
      <CircularProgress color="secondary" />
    </Hidden>
    <Hidden xlDown={hide}>
    <Typography variant="h4" component="h4" className={classes.text}>
      Extracted empirical distribution over query
    </Typography>
    <Histogram data={data} maxY={maxY}></Histogram>
    <Typography variant="h4" component="h4" className={classes.text}>
      Extracted items
    </Typography>
    <ObjectGrid objects={results} dois={doiResults}></ObjectGrid>
    </Hidden>
    <Hidden xlDown={noResults}>
        <Typography variant="h4" component="h4" className={classes.text}>
          No results found.
        </Typography>
    </Hidden>
    </div>
  );
}

export default Visualize;
