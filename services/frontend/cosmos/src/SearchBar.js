import React from 'react';
import TextField from '@material-ui/core/TextField';
import SearchIcon from '@material-ui/icons/Search';
//import MenuIcon from '@material-ui/icons/Menu';
import InputAdornment from '@material-ui/core/InputAdornment';

export default function SearchBar(props){
  const [values, setValues] = React.useState({
    query: '',
  });
  const handleChange = name => event => {
    setValues({ ...values, [name]: event.target.value });
  };

  return (
    <TextField
      id="outlined-full-width"
      label="Search"
      style={{ margin: 20 }}
      fullWidth
      placeholder=""
      helperText="Ask a question"
      margin="normal"
      onChange={handleChange('query')}
      variant="outlined"
      onKeyPress={(ev) => {
        if(ev.key === 'Enter') {
          props.enter_fn(values.query)
        }
      }}
      InputProps={{
        startAdornment: (
          <InputAdornment position="start">
            <SearchIcon />
          </InputAdornment>
        ),
      }}
      InputLabelProps={{
      shrink: true,
    }}/>)
}
