import React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import Typography from '@material-ui/core/Typography';
import SearchBar from './SearchBar.js'
import ObjectGrid from './ObjectGrid.js'
import Select from '@material-ui/core/Select';
import MenuItem from '@material-ui/core/MenuItem';
import FormControl from '@material-ui/core/FormControl';



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
  formControl: {
    margin: 20,
    minWidth: 120,
  },
}));



function Search() {
  const classes = useStyles();
  const [values, setValues] = React.useState({
    type: 'Body text',
    query: '',
  });
  const [results, setResults] = React.useState([])
  const [doiResults, setDoi] = React.useState([])
  function handleChange(event) {
    setValues(oldValues => ({
      ...oldValues,
      [event.target.name]: event.target.value,
    }));
  }
  function onEnter(query){
    setValues({...values, ['query']: query})
    setResults([])
    setDoi([])
    fetch(`http://localhost:5001/search?q=${encodeURIComponent(query)}&type=${encodeURIComponent(values.type)}`)
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
          })
      }
    })
  }
  return (
    <div className={classes.root}>
    <Typography variant="h3" component="h1" style={{margin: 20}}>
        Knowledge Search
    </Typography>
    <FormControl className={classes.formControl}>
    <Select
        value={values.type}
        onChange={handleChange}
        inputProps={{
          name: 'type',
          id: 'type-inp'
        }}
    >
      <MenuItem value={'Body text'}>Text</MenuItem>
      <MenuItem value={'equation'}>Equations</MenuItem>
      <MenuItem value={'table'}>Tables</MenuItem>
      <MenuItem value={'figure'}>Figures</MenuItem>
    </Select>
    </FormControl>
    <SearchBar enter_fn={onEnter}></SearchBar>
    <div className={classes.container}>
    <ObjectGrid objects={results} dois={doiResults}></ObjectGrid>
    </div>
    </div>
  );
}

export default Search;