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
  function handleChange(event) {
    setValues(oldValues => ({
      ...oldValues,
      [event.target.name]: event.target.value,
    }));
  }
  function onEnter(query){
    alert(query)
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
      <MenuItem value={'Equation'}>Equations</MenuItem>
      <MenuItem value={'Table'}>Tables</MenuItem>
      <MenuItem value={'Figure'}>Figures</MenuItem>
    </Select>
    </FormControl>
    <SearchBar enter_fn={onEnter}></SearchBar>
    <div className={classes.container}>
    <ObjectGrid></ObjectGrid>
    </div>
    </div>
  );
}

export default Search;